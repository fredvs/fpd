{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdbgdisasarm.pas  -  Native Freepascal debugger - arm Disassembler
 ---------------------------------------------------------------------------

 This unit contains an avr disassembler for the Native Freepascal debugger

 ---------------------------------------------------------------------------

 @created(9/11/2021)
 @lastmod($Date$)
 @author()

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
unit FpDbgDisasArm;
{$mode objfpc}{$H+}
interface

uses
  SysUtils, az_AArch64,
  FpDbgUtil, FpDbgInfo, DbgIntfBaseTypes, FpdMemoryTools, 
  {$ifdef FORCE_LAZLOGGER_DUMMY} LazLoggerDummy {$else} LazLoggerBase {$endif},
  FpDbgClasses;

type
  //The function Disassemble decodes the instruction at the given address.
  //Unrecognized instructions are assumed to be data statements [dw XXXX]

  TArmAsmDecoder = class;

  { TX86DisassemblerInstruction }

  { TAvrArmInstruction }

  TAvrArmInstruction = class(TDbgAsmInstruction)
  private const
    INSTR_CODEBIN_LEN = 4;
  private
    FAsmDecoder: TArmAsmDecoder;
    FAddress: TDBGPtr;
    FCodeBin: array[0..INSTR_CODEBIN_LEN-1] of byte;
    FFlags: set of (diCodeRead, diCodeReadError);
  protected
    procedure ReadCode; inline;
  public
    constructor Create(AAsmDecoder: TArmAsmDecoder);
    procedure SetAddress(AnAddress: TDBGPtr);
    function IsCallInstruction: boolean; override;
    function IsReturnInstruction: boolean; override;
    function IsLeaveStackFrame: boolean; override;
    function InstructionLength: Integer; override;
  end;

{ TArmAsmDecoder }

  TArmAsmDecoder = class(TDbgAsmDecoder)
  private const
    MaxPrologueSize = 64;  // Bytes, so ~32 instructions
    MaxEpilogueSize = MaxPrologueSize; // Perhaps a bit smaller, since the locals/parameters do not have to be initialized
    MAX_CODEBIN_LEN = MaxPrologueSize; // About 32 instructions
  private
    FProcess: TDbgProcess;
    FLastErrWasMem: Boolean;
    FCodeBin: array[0..MAX_CODEBIN_LEN-1] of byte;
    FLastInstr: TAvrArmInstruction;
    function FParsePrologue(AnAddress, AStartPC, AEndPC: TDBGPtr; out
      returnAddressOffset: word; out AnIsOutsideFrame: Boolean): Boolean;
    function FParseEpilogue(AnAddress, AStartPC, AEndPC: TDBGPtr; out
      returnAddressOffset: word; out AnIsOutsideFrame: Boolean): Boolean;
  protected
    function GetLastErrorWasMemReadErr: Boolean; override;
    function GetMaxInstrSize: integer; override;
    function GetMinInstrSize: integer; override;
    function GetCanReverseDisassemble: boolean; override;
    function ReadCodeAt(AnAddress: TDBGPtr; var ALen: Cardinal): Boolean; inline;

  public
    procedure Disassemble(var AAddress: Pointer; out ACodeBytes: String; out ACode: String); override;
    function GetInstructionInfo(AnAddress: TDBGPtr): TDbgAsmInstruction; override;

    // Don't use, ot really suited to AVR ABI
    function GetFunctionFrameInfo(AnAddress: TDBGPtr; out
      AnIsOutsideFrame: Boolean): Boolean; override;

    // Rather use the next function to locate the call return address.
    // AStartPC & AEndPC indicates proc limits to help with scanning for prologue/epilogue
    // returnAddressOffset gives the offset to return address relative to Y pointer (r28:r29) inside frame
    // else returnAddressOffset gives the offset to return address relative to SP
    function GetFunctionFrameReturnAddress(AnAddress, AStartPC, AEndPC: TDBGPtr; out
      returnAddressOffset: word; out AnIsOutsideFrame: Boolean): Boolean;

    constructor Create(AProcess: TDbgProcess); override;
    destructor Destroy;
  end;


implementation

uses
  StrUtils, LazClasses, Math;

{ TAvrArmInstruction }

procedure TAvrArmInstruction.ReadCode;
begin
  if not (diCodeRead in FFlags) then begin
     end;
end;

constructor TAvrArmInstruction.Create(AAsmDecoder: TArmAsmDecoder);
begin
  FAsmDecoder := AAsmDecoder;
  inherited Create;
  AddReference;
end;

procedure TAvrArmInstruction.SetAddress(AnAddress: TDBGPtr);
begin
  FAddress := AnAddress;
  FFlags := [];
end;

function TAvrArmInstruction.IsCallInstruction: boolean;
var
  LoByte, HiByte: byte;
begin
  Result := False;
 {
  ReadCode;
  LoByte := FCodeBin[0];
  HiByte := FCodeBin[1];
  if ((HiByte and $FE) = $94) and ((LoByte and $0E) = $0E) or // call
     ((HiByte = $95) and (LoByte in [$09, $19])) or           // icall / eicall
     ((HiByte and $D0) = $D0) then                            // rcall
}
    Result := true;

end;

function TAvrArmInstruction.IsReturnInstruction: boolean;
var
  LoByte, HiByte: byte;
begin
  Result := False;
 { ReadCode;
  LoByte := FCodeBin[0];
  HiByte := FCodeBin[1];
  if ((HiByte = $95) and (LoByte in [$08, $18])) then  // ret / reti
  }
    Result := true;
end;

function TAvrArmInstruction.IsLeaveStackFrame: boolean;
begin
  Result := false;
end;

function TAvrArmInstruction.InstructionLength: Integer;
var
  LoByte, HiByte: byte;
begin
  Result := 2;
  {
  ReadCode;
  LoByte := FCodeBin[0];
  HiByte := FCodeBin[1];
  if ((HiByte and $FE) = $94) and ((LoByte and $0E) in [$0C, $0E]) or   // jmp / call
     ((HiByte and $FE) in [$90, $92]) and ((LoByte and $0F) = $0) then  // lds / sts
    Result := 4;
}
end;

type
  TPrologueState = (psStart, psPush, psLoadSPL, psLoadSPH, psLoadSreg,
    psModifySPL, psModifySPH, psWriteSPH, psWriteSreg, psWriteSPL, psCopyParams);

function TArmAsmDecoder.FParsePrologue(AnAddress, AStartPC, AEndPC: TDBGPtr;
  out returnAddressOffset: word; out AnIsOutsideFrame: Boolean): Boolean;
var
  ADataLen: Cardinal;
  AData: PByte;
  opcode, frameOffset: word;
  d, k: byte;
  stackState: TPrologueState;
begin

end;

type
  // State sequence runs in reverse direction
  TEpilogueState = (esStart, esRet, esPop, esWriteSPH, esWriteSreg, esWriteSPL,
    esLoadSreg, esModifyFPH, esModifyFPL);

function TArmAsmDecoder.FParseEpilogue(AnAddress, AStartPC, AEndPC: TDBGPtr;
  out returnAddressOffset: word; out AnIsOutsideFrame: Boolean): Boolean;
var
  ADataLen: Cardinal;
  AData: PByte;
  opcode, frameOffset: word;
  d, k: byte;
  stackState: TEpilogueState;
begin
result := true;
end;

function TArmAsmDecoder.GetLastErrorWasMemReadErr: Boolean;
begin
  Result := FLastErrWasMem;
end;

function TArmAsmDecoder.GetMaxInstrSize: integer;
begin
  Result := 4;
end;

function TArmAsmDecoder.GetMinInstrSize: integer;
begin
  Result := 2;
end;

function TArmAsmDecoder.GetCanReverseDisassemble: boolean;
begin
  Result := true;
end;

function TArmAsmDecoder.ReadCodeAt(AnAddress: TDBGPtr; var ALen: Cardinal
  ): Boolean;
begin
  Result := FProcess.ReadData(AnAddress, ALen, FCodeBin[0], ALen);
  FLastErrWasMem := not Result;
end;

procedure TArmAsmDecoder.Disassemble(var AAddress: pointer; out
  ACodeBytes: String; out ACode: String);
 var
  Insn: TA64Instruction;
  OpCode: UInt32;
  S: string;
  Options: TAzoteOptions = [];
begin
   s := string(AAddress) ;
   OpCode := StrToInt(S);
 
   FillChar(Insn, SizeOf(TA64Instruction), #00);
   Insn.OpCode := OpCode;
   Insn.Options := Options ;
   DecodeInstruction(Insn);
 
   ACodeBytes := s;
   ACode := Insn.Syntax;
end;

function TArmAsmDecoder.GetInstructionInfo(AnAddress: TDBGPtr
  ): TDbgAsmInstruction;
begin
  if (FLastInstr = nil) or (FLastInstr.RefCount > 1) then begin
    ReleaseRefAndNil(FLastInstr);
    FLastInstr := TAvrArmInstruction.Create(Self);
  end;

  FLastInstr.SetAddress(AnAddress);
  Result := FLastInstr;
end;

function TArmAsmDecoder.GetFunctionFrameInfo(AnAddress: TDBGPtr; out
  AnIsOutsideFrame: Boolean): Boolean;
begin
  Result := False;
end;

function TArmAsmDecoder.GetFunctionFrameReturnAddress(AnAddress, AStartPC,
  AEndPC: TDBGPtr; out returnAddressOffset: word; out AnIsOutsideFrame: Boolean
  ): Boolean;
var
  ADataLen: Cardinal;
  AData: PByte;
begin
  { Cases to consider:
    A - if (AStartPC + MaxPrologueSize < AnAddress) and (AnAddress + MaxEpilogueSize < AEndPC)
        then currently inside stack frame. Parse prologue to figure out
        offset from frame pointer to return address.

    B - if (AStartPC + MaxPrologueSize < AnAddress)
        then possibly before final stack frame.  Need to parse prologue up to AnAddress
        to figure out how far the stack has moved since entry to calculate offset
        from SP to return address. If frame pointer has been configured before
        AnAddress, assume inside frame and return offset relative to frame pointer.

    C - if (AnAddress + MaxEpilogueSize < AEndPC)
        then possibly inside frame teardown.  Need to reverse parse epilogue up to AnAddress
        to figure out how much frame will unwind to calculate offset to return address.
        If frame pointer has been restored before AnAddress then ouside frame.
  }

end;

constructor TArmAsmDecoder.Create(AProcess: TDbgProcess);
begin
  FProcess := AProcess;
end;

destructor TArmAsmDecoder.Destroy;
begin
  ReleaseRefAndNil(FLastInstr);
  inherited Destroy;
end;

initialization
  DebugLogger.FindOrRegisterLogGroup('DBG_WARNINGS' {$IFDEF DBG_WARNINGS} , True {$ENDIF} );

end.
