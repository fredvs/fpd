{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdloop.pas  -  FP standalone debugger - Debugger main loop
 ---------------------------------------------------------------------------

 This unit contains the main loop of the debugger. It waits for a debug
 event and handles it

 ---------------------------------------------------------------------------

 @created(Mon Apr 10th WET 2006)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.nl>)

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit FPDLoop;

{$mode objfpc}{$H+}
interface

uses
  keyboard,
  Classes,
  SysUtils,
  FileUtil,
  LazFileUtils,
  LazUTF8,
  FpDbgInfo,
  FpDbgClasses,
  DbgIntfBaseTypes,
  FpDbgDwarf,
  FpdMemoryTools,
  CustApp;

type

  { TFPDLoop }

  TFPDLoop = class(TCustomApplication)
  private
    FLast: string;
    FMemReader: TDbgMemReader;
    FMemManager: TFpDbgMemManager;
    FMemConvertor: TFpDbgMemConvertor;
    procedure ShowDisas;
    procedure ShowCode;
    procedure GControllerExceptionEvent(var continue: Boolean; const ExceptionClass, ExceptionMessage: string);
    procedure GControllerCreateProcessEvent(var continue: Boolean);
    procedure GControllerHitBreakpointEvent(var continue: Boolean; const Breakpoint: TFpDbgBreakpoint; AnEventType: TFPDEvent; AMoreHitEventsPending: Boolean);
    procedure GControllerProcessExitEvent(ExitCode: DWord);
    procedure GControllerDebugInfoLoaded(Sender: TObject);
  protected
    procedure DoRun; override;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

const
  CHARS = ['=', '+', '-', '/', ':', ';', ' ', '\', '.', ',', '#', '{', '}', '@',
           '*', '%', '[', ']', '(', ')', '&', '~', '^', '$', '_', '"', '''', 
           '?', '!', '0'..'9', 'a'..'z', 'A'..'Z'];

implementation

uses
  FPDCommand,
  FpDbgUtil,
  FPDGlobal,
  FPDbgController,
  FpDbgCommon;

type

  { TPDDbgMemReader }

  TPDDbgMemReader = class(TDbgMemReader)
  protected
    function GetDbgProcess: TDbgProcess; override;
  end;


resourcestring
  sBreakpointReached = 'Breakpoint reached:';
  sProcessPaused     = 'Process paused.';
  sProcessExited     = 'Process ended with exit-code %d.';

{ TPDDbgMemReader }

function TPDDbgMemReader.GetDbgProcess: TDbgProcess;
begin
  Result := GController.CurrentProcess;
end;

{ TFPDLoop }

procedure TFPDLoop.GControllerExceptionEvent(var continue: Boolean; const ExceptionClass, ExceptionMessage: string);
begin
  if ExceptionMessage <> '' then
  begin
    writeln('Program raised exception class ''' + ExceptionClass + '''. Exception message:');
    writeln(ExceptionMessage);
  end
  else
    writeln('Program raised exception class ''' + ExceptionClass + '''.');

  if not continue then
  begin
    ShowCode;
    ShowDisas;
  end;
end;

procedure TFPDLoop.GControllerProcessExitEvent(ExitCode: DWord);
begin
  writeln(format(sProcessExited, [ExitCode]));
  StepNum  := 0;
  IniState := isNo;
end;

procedure TFPDLoop.GControllerDebugInfoLoaded(Sender: TObject);
var
  targetInfo: TTargetDescriptor;
begin
  WriteLn('Debug information loaded.');
  targetInfo := GController.CurrentProcess.LoaderList[0].TargetInfo;
  WriteLn('  Machine type: ', targetInfo.machineType);
  WriteLn('  OS: ', targetInfo.OS);
end;

procedure TFPDLoop.ShowDisas;
var
  a: TDbgPtr;
  Code, CodeBytes: string;
  CodeBin: array[0..20] of byte;
  p: Pointer;
  i: integer;
begin
  WriteLN();
  a     := GController.CurrentThread.GetInstructionPointerRegisterValue;
  for i := 0 to 5 do
  begin
    Write('  ', FormatAddress(a), ' ');

    if not GController.CurrentProcess.ReadData(a, sizeof(CodeBin), CodeBin) then
    begin
      //debugln('Disassemble: Failed to read memory at %s.', [FormatAddress(a)]);
      Code      := '??';
      CodeBytes := '??';
      Inc(a);
      Exit;
    end;
    p := @CodeBin;

    GController.CurrentProcess.Disassembler
      .Disassemble(p, CodeBytes, Code);

    WriteLN(' ', CodeBytes: 20, '    ', Code);
    Inc(a, PtrUInt(p) - PtrUInt(@CodeBin));
  end;
end;

procedure TFPDLoop.ShowCode;
var
  a: TDbgPtr;
  sym, symproc: TFpSymbol;
  S: TStringList;
  AName: string;
begin
  a := GController.CurrentThread.GetInstructionPointerRegisterValue;

  sym := GController.CurrentProcess.FindProcSymbol(a);
  if sym = nil then
  begin
    WriteLn('  ', trim(FormatAddress(a)), ' ???');
    Exit;
  end;

  symproc   := sym;
  while not (symproc.kind in [skProcedure, skFunction]) do
    symproc := symproc.Parent;

  if sym <> symproc then
  begin
    if symproc = nil then
      WriteLn('???')
    else
      WriteLn('  ', FormatAddress(a), '  ', sym.Name, ',  line ', sym.Line, ':',
      sym.Column, ' of ', ExtractFileName(sym.FileName));
  end;

  WriteLn('  ', FormatAddress(a), '  ', sym.Name, ',  line ', sym.Line, ':',
   sym.Column, ' of ', ExtractFileName(sym.FileName));

  AName := sym.Filename;
  if not FileExistsUTF8(AName) then
    if ExtractFilePath(AName) = '' then
    begin
      AName   := IncludeTrailingPathDelimiter(ExtractFilePath(GController.ExecutableFilename)) + AName;
      if not FileExistsUTF8(AName) then
        AName := '';
    end
    else
      AName := '';

  if AName = '' then
  begin
    WriteLn('   File not found');
    Exit;
  end;

  S := TStringList.Create;
  try
    S.LoadFromFile(UTF8ToSys(AName));
    if S.Count < sym.Line then
      WriteLn('   Line not found')
    else
      WriteLn('   ' + trim(S[sym.Line - 1]));
  except
    on E: Exception do
      WriteLn(trim(E.Message));
  end;
  S.Free;

  sym.ReleaseReference;
end;

procedure TFPDLoop.GControllerCreateProcessEvent(var continue: Boolean);
begin
  continue := False;
end;

procedure TFPDLoop.GControllerHitBreakpointEvent(var continue: Boolean; const Breakpoint: TFpDbgBreakpoint; AnEventType: TFPDEvent; AMoreHitEventsPending: Boolean);
begin
  if Assigned(Breakpoint) then
    writeln(sBreakpointReached)
  else
    writeln(sProcessPaused);

  if not continue then
  begin
    ShowCode;
    ShowDisas;
  end;
end;

function getkeys(prompt: string): string;
var
  s: string = '';
  b: Boolean;
  i, ih: integer;
  key: char = #00;
  kw: word;
  k: TKeyEvent;
  
  procedure removeline;
  var
  stmp: string = ' ';
  i : integer;
  begin
     for i  := 1 to length(s) + length(prompt) do
       stmp := stmp + ' ';
     Write(#13 + stmp); // clear current line
  end;
  
begin
  InitKeyBoard;
  ih := 0; // index history
  while Key <> #13 do // enter not pressed
  begin
    K   := GetKeyEvent;
    K   := TranslateKeyEvent(K);
    kw  := GetKeyEventCode(K);
    key := GetKeyEventChar(K);
    
    case kw of
      KbdUp: if length(HistoryCommand) > 0 then // Up arrow key
        begin
          removeline;
          if ih < length(HistoryCommand) then
          begin
           s := HistoryCommand[length(HistoryCommand) -1 - ih];
           Inc(ih);
          end else s := '' ; 
          
          Write(#13 + prompt + s);
        end;

      kbdDown: if length(HistoryCommand) > 0 then // Down arrow key
        begin
          removeline;
          if (ih > 0 ) then
          begin
            s    := HistoryCommand[length(HistoryCommand) - ih];
            Dec(ih);
          end  else
          begin
           ih := 0;
           s := '';
          end; 
          Write(#13 + prompt + s); 
        end;
                
      3592: // backspace
      begin 
        removeline;
        s := system.copy(s, 1, length(s) - 1);
        Write(#13 + prompt + s);
      end;
      else
        if (key in CHARS) then
          begin
            S := s + char(key);
            Write(key);
          end;
    end;
  end;

  DoneKeyBoard;

  if trim(s) <> '' then
  begin
    b     := False;
    for i := 0 to length(HistoryCommand) - 1 do
      if HistoryCommand[i] = trim(s) then
        b := True;
    if b = False then
    begin
      setlength(HistoryCommand, length(HistoryCommand) + 1);
      HistoryCommand[length(HistoryCommand) - 1] := trim(s);
    end;
  end;

  Result := trim(s);

end;

procedure TFPDLoop.DoRun;
var
  S: string = '';
  b: Boolean;
begin
  S := 'FPD ' + IntToStr(StepNum) + ' ~> ';

  Write(S);
  s := getkeys(s);

  writeln();

  if S <> '' then
    FLast := S;
  if FLast <> '' then
  begin
    HandleCommand(FLast, b);

    if b then
      Inc(StepNum);
    while b do
    begin
      GController.ProcessLoop;
      GController.SendEvents(b);
    end;
  end;
 end;

procedure TFPDLoop.Initialize;
var
i : integer = 0;
begin
  inherited Initialize;
  StepNum       := 0;
  FMemReader    := TPDDbgMemReader.Create;
  FMemConvertor := TFpDbgMemConvertorLittleEndian.Create;
  FMemManager   := TFpDbgMemManager.Create(FMemReader, FMemConvertor);
  GController   := TDbgController.Create(FMemManager);

  if ParamCount > 0 then
    for i := 1 to ParamCount do
    if fileexists(ParamStr(i)) then
    begin
      GController.ExecutableFilename := ParamStr(i);
      WriteLN('Using file: ', GController.ExecutableFilename);
    end;
    
  //TODO: Maybe DebugLogger.OnLog ....
  //GController.OnLog:=@OnLog;
  GController.OnHitBreakpointEvent := @GControllerHitBreakpointEvent;
  GController.OnCreateProcessEvent := @GControllerCreateProcessEvent;
  GController.OnExceptionEvent     := @GControllerExceptionEvent;
  GController.OnProcessExitEvent   := @GControllerProcessExitEvent;
  GController.OnDebugInfoLoaded    := @GControllerDebugInfoLoaded;

end;

destructor TFPDLoop.Destroy;
begin
  FMemManager.Free;
  FMemReader.Free;
  FMemConvertor.Free;
  GController.Free;
  inherited Destroy;
end;

initialization
  CustomApplication := TFPDLoop.Create(nil);

finalization
  CustomApplication.Free;
end.