unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  ComCtrls, ExtCtrls, IniFiles, Process, LCLType, Types;//, Clipbrd;


const
  PDT_NULL = 0;    
  PDT_Speaker = 1;
  PDT_Player = 2;   
  PDT_Microfone = 4;
  PDT_Recorder  = 8;
  PDT_Monitor = 16;
  PDT_LoopRec = 32;
  PDT_LoopSpk = 64;    
  PDT_Loop = 128;
  PDT_UnknownDev = 256;

type
  TPulseDevice = record
    Name: String;
    Ident: String;
    Device_Name: String;
    App_Name: String;
    Media_Name: String;
    Loop_In: String;
    Loop_Out: String;
    Prog_Name: String;
    VSink: Boolean;
    Module_ID: String;
    Typ: Word;
    ID: String;
    MicID: String;
    SpkID: String;
    PlayingOn: String;
    RecordingFrom: String;
    Volume: String;
    Vol_Muted: Boolean;
  end;

type
  TCable = record
    Input: Integer;
    Output: Integer;
    Start: TPoint;
    Goal: TPoint;
    Lower: TPoint;
    Higher: TPoint;
    Color: Integer;
  end;

type TPulseDevices = Array of TPulseDevice;

type
  TDevice = class
  private
    FMainCaption, FSecCaption: String;
    FTextHeight: Integer;
    FWidth, FHeight: Integer;
    FX, FY: Integer;
    FSelected: Boolean;
    FOldD: TPulseDevice;
    procedure FSetCaption;
    procedure SetFX(AX: Integer);
    procedure SetFY(AY: Integer);
    function GetWidth: Integer;     
    function GetHeight: Integer;
  public
    SelColor: Integer;
    MouseBtnPressed: Boolean;
    D: TPulseDevice;
    property X: Integer read FX write SetFX;      
    property Y: Integer read FY write SetFY;
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Selected: Boolean read FSelected write FSelected;
    constructor Create(AD: TPulseDevice);
    procedure Paint;
  end;

type
  TDeviceMngt = class
    private
      FImage: TBitmap;
      FRepainting: Boolean;
      FCables: Array of TCable;
      FCircleCallCheck: Array of Integer;
      FDeviceSelected: Integer;
      FLastMoveP: TPoint;
      FStartMoveP: TPoint;
      FDefaultSink: String;
      FDefaultMic: String;
      function FCheckSelectedCable(ACable: TCable; Mouse: TPoint): Boolean;
      procedure FGetLinePoint(ADev: TDevice; var PlyP: TPoint; var RecP: TPoint);
      procedure FDrawSingleLine(Lower, Higher: TPoint; AColor: Integer);
      procedure FDrawOneConnection(Lower, Higher: TPoint; AColor: Integer);
      procedure FDrawDeviceLines(var Con: TCable);
      procedure FCalcConnections;
      procedure FDrawConnections;
    public
      Devs: Array of TDevice;
      PD: TPulseDevices;    
      property DefaultSink: String read FDefaultSink;
      property DefaultMic: String read FDefaultMic;
      constructor Create;
      function CheckSelectedDevice(AID: Integer; Mouse: TPoint): Boolean;
      procedure Repaint;
      procedure LoadFromPulse;
      procedure LoadFromFile(AFileName: String);    
      procedure SetMute(AID: Integer; Mute: Boolean);
      procedure SetVol(AID: Integer; AVol: Integer);
      //Cabel-Mngt:
      procedure InitCableMove(P: TPoint);
      procedure DoCableMove(P: TPoint);
      procedure EndCableMove(P: TPoint);
      function SaveToFile(AFileName: String): Boolean;
      destructor Free;
  end;

type

  { TMainFRM }

  TMainFRM = class(TForm)
    CalcLab: TLabel;
    BI: TImage;
    MainMenu: TMainMenu;
    M1_File: TMenuItem;
    M01_Load: TMenuItem;
    M01_SaveAs: TMenuItem;
    M01_Exit: TMenuItem;
    M5_Reload: TMenuItem;
    M2_Devices: TMenuItem;
    M02_AddLoop: TMenuItem;
    M02_AddVSink: TMenuItem;
    M02_Line: TMenuItem;
    M02_Del_Loops: TMenuItem;
    M02_Del_Sinks: TMenuItem;
    M02_Line2: TMenuItem;
    M02_Dell_All: TMenuItem;
    M3_DefSpeakers: TMenuItem;
    M4_DefMicrofone: TMenuItem;
    M01_Options: TMenuItem;
    M01_OP_DarkM: TMenuItem;
    M01_OP_AutoReload: TMenuItem;
    M01_OP_OnTop: TMenuItem;
    M01_SEP01: TMenuItem;
    M01_SEP02: TMenuItem;
    M01_LoadRecent: TMenuItem;
    RC_Del: TMenuItem;
    PB_NewLine: TPaintBox;
    RC: TPopupMenu;
    SB: TScrollBox;
    AutoT: TTimer;
    procedure AutoTTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure M01_ExitClick(Sender: TObject);
    procedure M01_LoadClick(Sender: TObject);
    procedure M01_NewClick(Sender: TObject);
    procedure M01_OP_AutoReloadClick(Sender: TObject);
    procedure M01_OP_OnTopClick(Sender: TObject);
    procedure M01_SaveAsClick(Sender: TObject);
    procedure M02_AddLoopClick(Sender: TObject);
    procedure M02_AddVSinkClick(Sender: TObject);
    procedure M02_Dell_AllClick(Sender: TObject);
    procedure M02_Del_LoopsClick(Sender: TObject);
    procedure M02_Del_SinksClick(Sender: TObject);
    procedure M5_ReloadClick(Sender: TObject);
    procedure M01_OP_DarkMClick(Sender: TObject);
    procedure PB_NewLineMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PB_NewLineMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure PB_NewLineMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure RC_DelClick(Sender: TObject);
    procedure SBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBMouseLeave(Sender: TObject);
    procedure SBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    AutoT_Cnt: Byte;
    RC_Dev: Integer;
    RecentFiles: TStringList;
    procedure SetDarkMode(IsDark: Boolean);
    procedure UpdateLoadMenu;
    procedure AddLoadMenu(AFile: String);
  public
    procedure M3_ItemClick(Sender: TObject);
    procedure M4_ItemClick(Sender: TObject);   
    procedure M01_LoadRecentClick(Sender: TObject);
  end;

type
  TStrArray = Array of String;

var
  MainFRM: TMainFRM;
  Dev: TDeviceMngt;
  D: TDevice;
  AD: TPulseDevice;
  //ReloadString, ändert er sich, haben sich die Devices geändert:
  LoadComp: String;
  //Farben:
  clLines: TColor;
  clBackgr: TColor;

implementation

{$R *.frm}

procedure AddMenuItem(AItem: TMenuItem; ACaption: String; const AOnClick: TNotifyEvent = nil);
var
  AM: TMenuItem;
  I: Integer;
begin
  AM := TMenuItem.Create(MainFRM.MainMenu);
  I := AItem.Count;
  AItem.Add(AM);
  AM.Caption := ACaption;
  AM.Tag := I;
  If AOnClick <> nil then
    AM.OnClick := AOnClick;
end;

procedure GetProcess(Acmd: String; LstResult:TStringList);
const
  BUF_SIZE = 2048;
var
  AProcess     : TProcess;
  OutputStream : TStream;
  BytesRead    : longint;
  Buffer       : array[1..BUF_SIZE] of byte;
begin
  LstResult.Clear;
  AProcess := TProcess.Create(nil);
  AProcess.Executable := '/bin/sh';
  AProcess.Parameters.Add('-c');
  AProcess.Parameters.Add('export LC_ALL=C && '+Acmd +' && unset LC_ALL');
  AProcess.Options := [poUsePipes];
  AProcess.Execute;
  OutputStream := TMemoryStream.Create;

  repeat
    BytesRead := AProcess.Output.Read(Buffer, BUF_SIZE);
    OutputStream.Write(Buffer, BytesRead)
  until BytesRead = 0;

  AProcess.Terminate(0);
  AProcess.Free;
  OutputStream.Position := 0;
  LstResult.LoadFromStream(OutputStream);
  OutputStream.Free;
end;

function RunCMD(const Cmd: String): String;
var
  TS: TStringList;
begin      
  Result := '';
  TS := TStringList.Create;
  try
    GetProcess(Cmd,TS);
    Result := TS.Text;
  finally
    TS.Free;
  end;
end;

function ReloadPulseDevices(var PD: TPulseDevices; var DefaultSink: String; var DefaultMic: String): Boolean;
  procedure ResetPulseDevice(var AD: TPulseDevice);
    begin
      AD.Ident := '';
      AD.ID := '';
      AD.PlayingOn := '';
      AD.RecordingFrom := '';
      AD.Name := '';
      AD.Device_Name := '';
      AD.App_Name := '';
      AD.Prog_Name := '';
      AD.Media_Name := '';
      AD.VSink := False;
      AD.Volume := '';
      AD.Typ := PDT_NULL;
    end;
  function CopyFromPos(Source: String; const FromStr: String): String;
    var
      I: Integer;
    begin
      I := Pos(FromStr,Source)+Length(FromStr);
      Result := Copy(Source,I,Length(Source)-I+1);
    end;
  function IsADevice(StrToCheck: String): Boolean;
    begin
      Result := False;
      if (Pos('#',StrToCheck) > 0) and (Pos(#9,StrToCheck) = 0) then
        Result := True;
    end;
  function IsNewDevice(StrToCheck: String): Boolean;
    begin
      Result := False;
      if (Pos('#',StrToCheck) > 0) and (
         (Pos('Source ',StrToCheck) > 0) or (Pos('Sink ',StrToCheck) > 0)) then
           Result := True;
    end;
var
  SL: TStringList;
  I, J: Integer;
  ND: TPulseDevice;
  Comp: String;
begin
  SetLength(PD,0);
  SL := tStringList.Create;
  try
    //Get Default Devices:
    GetProcess('pacmd stat',SL);
    DefaultSink := '';
    DefaultMic := '';
    If SL.Count > 0 then
      for I := 0 to SL.Count -1 do
        if (DefaultSink <> '') and (DefaultMic <> '') then
          break Else
        if Pos('Default sink name: ',SL[I]) > 0 then
          DefaultSink := CopyFromPos(SL[I],'Default sink name: ') Else
        if Pos('Default source name: ',SL[I]) > 0 then
          DefaultMic := CopyFromPos(SL[I],'Default source name: ');
    Comp := DefaultSink;
    Comp := Comp + DefaultMic;
    //Get Device Data:
    SL.Clear;
    GetProcess('pactl list',SL);
    I := 0;
    If SL.Count > 0 then
      repeat
        if IsNewDevice(SL[I]) = True then
          begin
            ResetPulseDevice(ND);
            ND.ID := CopyFromPos(SL[I],'#');
            if (Pos('Sink Input ',SL[I]) > 0) then
              ND.Typ := PDT_Player Else
            if (Pos('Source Output ',SL[I]) > 0) then
              ND.Typ := PDT_Recorder Else
              ND.Typ := PDT_UnknownDev;
            Comp := Comp + IntToStr(ND.Typ);
            for J := I to SL.Count -1 do
              begin
                If Pos('Name: ',SL[I]) > 0 then
                  begin
                    ND.Device_Name := CopyFromPos(SL[I],'Name: ');
                    Comp := Comp + ND.Device_Name;
                    If Copy(ND.Device_Name,1,6) = 'VSINK_' then
                      ND.VSink := True;
                    If ND.Typ = PDT_UnknownDev then
                      if (Pos('.monitor',SL[I]) > 0) then
                        ND.Typ := PDT_Monitor Else
                      if (Pos('alsa_input',SL[I]) > 0) then
                        begin
                          ND.Typ := PDT_Microfone;
                          ND.MicID := ND.ID;
                        end  Else
                        begin
                          ND.Typ := PDT_Speaker;
                          ND.SpkID := ND.ID;
                        end;
                  end;
                ND.Ident := ND.Device_Name;
                Comp := Comp + ND.Ident;
                if Pos('Mute: ',SL[I]) > 0 then
                  begin
                    ND.Vol_Muted := CopyFromPos(SL[I],'Mute: ') = 'yes';
                    If ND.Vol_Muted then
                      Comp := Comp + 'Muted';
                  end;
                If Pos('Sink: ',SL[I]) > 0 then
                  begin
                    ND.PlayingOn := CopyFromPos(SL[I],'Sink: ');
                    Comp := Comp + ND.PlayingOn;
                  end;
                If Pos('Source: ',SL[I]) > 0 then
                  begin
                    ND.RecordingFrom := CopyFromPos(SL[I],'Source: ');     
                    Comp := Comp + ND.RecordingFrom;
                  end;
                If Pos('device.description = "',SL[I]) > 0 then
                  begin
                    ND.Name := CopyFromPos(SL[I],'device.description = "');
                    If Length(ND.Name) > 0 then
                      begin
                        SetLength(ND.Name,Length(ND.Name)-1);
                        Comp := Comp + ND.Name;
                      end;
                  end;
                If Pos('application.name = "',SL[I]) > 0 then
                  begin
                    ND.App_Name := CopyFromPos(SL[I],'application.name = "');
                    If Length(ND.App_Name) > 0 then
                      begin
                        SetLength(ND.App_Name,Length(ND.App_Name)-1);
                        Comp := Comp + ND.App_Name;
                      end;
                  end;
                If Pos('media.name = "',SL[I]) > 0 then
                  begin
                    ND.Media_Name := CopyFromPos(SL[I],'media.name = "');
                    If Length(ND.Media_Name) > 0 then
                      begin
                        SetLength(ND.Media_Name,Length(ND.Media_Name)-1);
                        Comp := Comp + ND.Media_Name;
                      end;
                    If (ND.Typ = PDT_Recorder) and (Copy(ND.Media_Name,1,12)='Loopback to ') then
                      begin
                        ND.Typ := PDT_LoopRec;
                        Delete(ND.Media_Name,1,12);
                      end Else
                    If ND.Typ = PDT_Player then
                      If Copy(ND.Media_Name,1,12)='Loopback of ' then
                        begin
                          ND.Typ := PDT_LoopSpk;
                          Delete(ND.Media_Name,1,12);
                        end Else
                      If Copy(ND.Media_Name,1,14)='Loopback from ' then
                        begin
                          ND.Typ := PDT_LoopSpk;
                          Delete(ND.Media_Name,1,14);
                        end;
                  end;
                If Pos('application.process.binary = "',SL[I]) > 0 then
                  begin
                    ND.Prog_Name := CopyFromPos(SL[I],'application.process.binary = "');
                    If Length(ND.Prog_Name) > 0 then
                      begin
                        SetLength(ND.Prog_Name,Length(ND.Prog_Name)-1);
                        Comp := Comp + ND.Prog_Name;
                      end;
                  end;
                If Pos('Owner Module: ',SL[I]) > 0 then
                  begin
                    ND.Module_ID := CopyFromPos(SL[I],'Owner Module: ');
                    Comp := Comp + ND.Module_ID;
                  end;
                If (ND.Volume = '') and (
                (Pos('Volume: front-left: ',SL[I]) > 0) or   
                (Pos('Volume: mono: ',SL[I]) > 0) or
                (Pos('Base Volume: ',SL[I]) > 0) ) then
                  begin
                    ND.Volume := CopyFromPos(SL[I],' / ');
                    ND.Volume := Copy(ND.Volume,1,Pos('%',ND.Volume)-1);   
                    Comp := Comp + ND.Volume;
                  end;
                If ND.Volume <> '' then
                  ND.Volume := StringReplace(ND.Volume,' ','',[rfreplaceall]);
                If I < SL.Count -1 then
                  Inc(I) Else
                  break;
                if IsADevice(SL[I]) = True then
                  begin
                    Dec(I);
                    break;
                  end;
            end;
            if ND.Prog_Name <> 'pavucontrol' then
              begin
                //Setze Namen je nach Typ:
                if ND.Typ = PDT_Player then
                  ND.Name := ND.Prog_Name+': '+ND.Media_Name Else
                if ND.Typ = PDT_Recorder then
                  ND.Name := ND.Prog_Name+': '+ND.Media_Name;
                If ND.Ident = '' then
                  ND.Ident := ND.Name;
                SetLength(PD,Length(PD)+1);
                PD[Length(PD)-1] := ND;
              end;
          end;
        Inc(I);
      until I >= SL.Count;
    if Length(PD) > 0 then
      begin
    //Fasse Loopback zu einem Device zusammen:
        for I := 0 to Length(PD) -1 do
          if PD[I].Typ = PDT_LoopSpk then
            for J := 0 to Length(PD) -1 do
              if (PD[J].Typ = PDT_LoopRec) and (PD[I].Module_ID = PD[J].Module_ID) then
                begin
                  PD[I].Loop_In := PD[I].Media_Name;
                  PD[I].Loop_Out := PD[J].Media_Name;   
                  PD[I].Ident := PD[I].Loop_In+'-->'+PD[I].Loop_Out;
                  PD[I].MicID := PD[J].ID;
                  PD[I].RecordingFrom := PD[J].RecordingFrom;
                  PD[I].Typ := PDT_Loop;
                  ResetPulseDevice(PD[J]);
                  break;
                end;
        //Lerre leere Devices:
        I := 0;
        repeat
          If PD[I].ID = '' then
            begin
              if I < Length(PD) -1 then
                for J := I to Length(PD) -2 do
                  PD[J] := PD[J+1];
              SetLength(PD,Length(PD)-1);
            end Else
          Inc(I);
        until I >= Length(PD);
      end;
  finally
    SL.Free;
  end;
  Result := Comp <> LoadComp;
  LoadComp := Comp;
end;     

constructor TDeviceMngt.Create;
begin
  FImage := TBitmap.Create;
  FDeviceSelected := -1;
end;            

procedure TDeviceMngt.FGetLinePoint(ADev: TDevice; var PlyP: TPoint; var RecP: TPoint);
begin
  PlyP.X := -1;
  PlyP.Y := -1;
  RecP.X := -1;
  RecP.Y := -1;
  //Eingangspfeil (offen) Links / Oben (Audioeingabe / Lautsprecher):
  if (PDT_Speaker and ADev.D.Typ <> 0) then
    begin
      PlyP.X := ADev.X;
      PlyP.Y := ADev.Y + 6;
    end;    
  //Ausgangspfeil (offen) Rechts / Oben (Audioausgabe / Player):
  if (PDT_Player and ADev.D.Typ <> 0) or (PDT_Loop and ADev.D.Typ <> 0) then
    begin
      PlyP.X := ADev.X + ADev.Width;
      PlyP.Y := ADev.Y + 6;
    end;
  //Eingangspfeil (geschlossen) Rechts / Unten (Audioeingabe / Recorder):
  if (PDT_Recorder and ADev.D.Typ <> 0) or (PDT_Loop and ADev.D.Typ <> 0) then
    begin
      RecP.X := ADev.X + ADev.Width;
      RecP.Y := ADev.Y + ADev.Height -6;
    end;
  //Ausgangspfeil Unten (Audioausgabe / Mikrofon / Monitor):
  if (PDT_Microfone and ADev.D.Typ <> 0) or (PDT_Monitor and ADev.D.Typ <> 0) then
    begin
      RecP.X := ADev.X;
      RecP.Y := ADev.Y + ADev.Height -6;
    end;
  If RecP.X = -1 then
    begin
      RecP.X := PlyP.X;    
      RecP.Y := PlyP.Y;
    end;
  If PlyP.X = -1 then
    begin
      PlyP.X := RecP.X;    
      PlyP.Y := RecP.Y;
    end;
end;

procedure GetLowerHigher(Start, Goal: TPoint; var Lower, Higher: TPoint);
begin    
  If Start.X < Goal.X then
    begin
      Lower.X := Start.X;
      Lower.Y := Start.Y;
      Higher.X := Goal.X;
      Higher.Y := Goal.Y;
    end Else
    begin
      Lower.X := Goal.X;
      Lower.Y := Goal.Y;
      Higher.X := Start.X;
      Higher.Y := Start.Y;
    end;
end;

procedure TDeviceMngt.FDrawSingleLine(Lower, Higher: TPoint; AColor: Integer);
begin
  MainFrm.PB_NewLine.Canvas.Pen.Color := AColor;
  MainFrm.PB_NewLine.Canvas.Pen.Width := 3;
  MainFrm.PB_NewLine.Canvas.Line(Lower.X,Lower.Y,Higher.X,Higher.Y);
end;

procedure TDeviceMngt.FDrawOneConnection(Lower, Higher: TPoint; AColor: Integer);
begin
  FImage.Canvas.Pen.Color := AColor;
  FImage.Canvas.Pen.Width := 3;
  //Device -->
  FImage.Canvas.Line(Lower.X,Lower.Y,Lower.X+( (Higher.X - Lower.X) div 2)+20,Lower.Y);
  //Line
  FImage.Canvas.Line(Lower.X+( (Higher.X - Lower.X) div 2)+20,Lower.Y,Higher.X-30,Higher.Y);
  //--> Device
  FImage.Canvas.Line(Higher.X,Higher.Y,Higher.X-30,Higher.Y);
end;

procedure TDeviceMngt.FDrawDeviceLines(var Con: TCable);
var
  P1, P2: TPoint;
begin
  FGetLinePoint(Devs[Con.Input],P1,P2);
  if Devs[Con.Input].D.Typ = PDT_Loop then
    begin
      if (Devs[Con.Output].D.Typ = PDT_Monitor) or (Devs[Con.Output].D.Typ = PDT_Microfone) then
        Con.Start := P2 Else
        Con.Start := P1;
    end Else
      Con.Start := P2;
  FGetLinePoint(Devs[Con.Output],P1,P2);
  if Devs[Con.Output].D.Typ = PDT_Loop then
    begin
      if (Devs[Con.Input].D.Typ = PDT_Monitor) or (Devs[Con.Input].D.Typ = PDT_Microfone) then
        Con.Goal := P2 Else
        Con.Goal := P1;
    end Else
      Con.Goal := P2;
  GetLowerHigher(Con.Start,Con.Goal,Con.Lower,Con.Higher);
  FDrawOneConnection(Con.Lower, Con.Higher,Con.Color);
end;

procedure TDeviceMngt.FCalcConnections;
var
  I, J: Integer;
begin
  SetLength(FCables,0);
  if Length(Devs) > 0 then
    for I := 0 to Length(Devs) -1 do
      begin
        if (PDT_Player and PD[I].Typ <> 0) or (PDT_Loop and PD[I].Typ <> 0) then
          begin
            If PD[I].SpkID <> '' then
              for J := 0 to Length(Devs) -1 do
                if (PD[J].ID = PD[I].PlayingOn) and (PDT_Speaker and PD[J].Typ <> 0) then
                  begin
                    SetLength(FCables,Length(FCables)+1);
                    FCables[Length(FCables)-1].Input := I;
                    FCables[Length(FCables)-1].Output := J;
                    if not (PDT_Loop and PD[I].Typ <> 0) then
                      break;
                  end;
          end;
        if (PDT_Recorder and PD[I].Typ <> 0) or (PDT_Loop and PD[I].Typ <> 0) then
          begin
            If PD[I].MicID <> '' then
              for J := 0 to Length(Devs) -1 do
                if (PD[J].ID = PD[I].RecordingFrom) and
                   ((PDT_Microfone and PD[J].Typ <> 0) or (PDT_Monitor and PD[J].Typ <> 0)) then
                  begin
                    SetLength(FCables,Length(FCables)+1);
                    FCables[Length(FCables)-1].Output := I;
                    FCables[Length(FCables)-1].Input := J;
                    break;
                  end;
          end;
      end;
end;

procedure TDeviceMngt.FDrawConnections;
var
  I: Integer;
  ColoredLine: Boolean;
begin
  If Length(FCables) > 0 then
    begin
      ColoredLine := False;
      for I := 0 to Length(FCables) -1 do
        if FCables[I].Color <> clLines then     
            ColoredLine := true Else
            FDrawDeviceLines(FCables[I]);
      If ColoredLine = True then
        for I := 0 to Length(FCables) -1 do
          if FCables[I].Color <> clLines then
            FDrawDeviceLines(FCables[I]);
    end;
end;

procedure TDeviceMngt.Repaint;
  procedure MarkDevice(ADev: Integer; ACol: Integer; const FollowBack: Boolean = True; const FollowUp: Boolean = False; const RepeatedCall: Boolean = False);
  var
    I: Integer;
  begin
    if RepeatedCall = False then
      SetLength(FCircleCallCheck,0);
    if Length(FCircleCallCheck) > 0 then
      for I := 0 to Length(FCircleCallCheck) -1 do
        if FCircleCallCheck[I] = ADev then
          exit;
    SetLength(FCircleCallCheck, Length(FCircleCallCheck) +1);
    FCircleCallCheck[Length(FCircleCallCheck)-1] := ADev;
    If Devs[ADev].Selected = False then
      begin
        Devs[ADev].SelColor := ACol;
        Devs[ADev].Selected := True;
      end;
    if FollowUp = True then
      begin
        if PD[ADev].Typ = PDT_Speaker then
          begin
            for I := 0 to Length(PD) -1 do
              if (PDT_Monitor and PD[I].Typ <> 0) and (PD[ADev].Device_Name+ '.monitor' = PD[I].Device_Name) then
                begin
                  MarkDevice(I,ACol,FollowBack,FollowUp, True);
                  break;
                end;
          end Else
        if PD[ADev].Typ = PDT_Loop then
          begin
            for I := 0 to Length(PD) -1 do
              if (PD[ADev].Loop_Out = PD[I].Device_Name) then
                begin
                  MarkDevice(I,ACol,FollowBack,FollowUp, True);
                  break;
                end;
          end;
        for I := 0 to Length(FCables) -1 do
          if FCables[I].Input = ADev then
            begin
              FCables[I].Color := ACol;
              MarkDevice(FCables[I].Output,ACol,FollowBack,FollowUp, True);
            end;
      end;
    if FollowBack = True then
      begin
        if PD[ADev].Typ = PDT_Monitor then
          begin
            for I := 0 to Length(PD) -1 do
              if (PDT_Speaker and PD[I].Typ <> 0) and (PD[ADev].Device_Name = PD[I].Device_Name+ '.monitor') then
                begin
                  MarkDevice(I,ACol,FollowBack,FollowUp, True);
                  break;
                end;
          end Else
        if PD[ADev].Typ = PDT_Loop then
          begin
            for I := 0 to Length(PD) -1 do
              if (PD[ADev].Loop_In = PD[I].Device_Name) then
                begin
                  MarkDevice(I,ACol,FollowBack,FollowUp, True);
                  break;
                end;
          end;
        for I := 0 to Length(FCables) -1 do
          if FCables[I].Output = ADev then
            begin
              FCables[I].Color := ACol;
              MarkDevice(FCables[I].Input,ACol,FollowBack,FollowUp, True);
            end;
      end;
  end;
var
  I, J : Integer;
  MaxLeftWidth, MaxRightWidth: Integer;
  LeftH, RightH: Integer;
begin
  if (Length(PD) > 0) and (FRepainting = False) then
    begin
      FRepainting := True;
      MaxLeftWidth := 0;
      MaxRightWidth := 0;
      for I := 0 to Length(PD) -1 do
        if (PDT_Player and PD[I].Typ <> 0) or
           (PDT_Loop and PD[I].Typ <> 0) or
           (PDT_Recorder and PD[I].Typ <> 0) then
             begin
               If MaxLeftWidth < Devs[I].Width then
                 MaxLeftWidth := Devs[I].Width;
             end else
             begin       
               If MaxRightWidth < Devs[I].Width then
                 MaxRightWidth := Devs[I].Width;
             end;
      MainFRM.Constraints.MinWidth := MaxLeftWidth + MaxRightWidth + 150;
      If MainFrm.Width < MainFRM.Constraints.MinWidth then
        MainFrm.Width := MainFRM.Constraints.MinWidth;
      LeftH := 10;
      RightH := 10;
      for I := 0 to Length(PD) -1 do
        begin
          if PDT_Speaker and PD[I].Typ <> 0 then
            begin
              Devs[I].Y := RightH;
              RightH := RightH + (Devs[I].Height + 20);
              Devs[I].X := MainFrm.Width - MaxRightWidth - 30;
              for J := 0 to Length(PD) -1 do
                if (PDT_Monitor and PD[J].Typ <> 0) and (PD[J].Device_Name = PD[I].Device_Name + '.monitor') then
                  begin
                    RightH := RightH -23;
                    Devs[J].Y := RightH;
                    RightH := RightH + (Devs[J].Height + 20); 
                    Devs[J].X := MainFrm.Width - MaxRightWidth - 30;
                    break;
                  end;
            end Else
          if PDT_Player and PD[I].Typ <> 0 then
            begin
              Devs[I].Y := LeftH;
              LeftH := LeftH + (Devs[I].Height + 20);
              Devs[I].X := 10;
            end;
        end;
      for I := 0 to Length(PD) -1 do
        begin
          if PDT_Loop and PD[I].Typ <> 0 then
            begin              
              Devs[I].Y := LeftH;
              Devs[I].X := 10;
              LeftH := LeftH + (Devs[I].Height + 20);
            end;
        end;      
      if LeftH < RightH then
        LeftH := RightH Else
        RightH := LeftH;
      for I := 0 to Length(PD) -1 do
        begin
          if PDT_Recorder and PD[I].Typ <> 0 then
            begin    
              Devs[I].Y := LeftH;
              LeftH := LeftH + (Devs[I].Height + 20);
              Devs[I].X := 10;
            end Else
          if PDT_Microfone and PD[I].Typ <> 0 then
            begin
              Devs[I].Y := RightH;
              Devs[I].X := MainFrm.Width - MaxRightWidth - 30;
              RightH := RightH + (Devs[I].Height + 20);
            end;
        end;   
      if LeftH < RightH then
        LeftH := RightH;
      MainFrm.Update;
      I := MainFrm.ClientWidth;
      FImage.Width := MainFrm.ClientWidth;
      if MainFrm.ClientHeight-2 < LeftH then
        FImage.Height := LeftH Else
        FImage.Height := MainFrm.ClientHeight-2;   
      FImage.Canvas.Brush.Color := clBackgr;
      FImage.Canvas.FillRect(0, 0, FImage.Width, FImage.Height);
      MainFRM.BI.Width := FImage.Width;
      MainFRM.BI.Height := FImage.Height;
      MainFRM.BI.Picture.Bitmap.SetSize(FImage.Width, FImage.Height);
      MainFRM.PB_NewLine.Width := FImage.Width;
      MainFRM.PB_NewLine.Height := FImage.Height;    
      FCalcConnections;
      for I := 0 to Length(FCables) -1 do
        FCables[I].Color := clLines;    
      //Wenn Device Ausgewählt: Zeichne die Devices in Farbe:
      If (FDeviceSelected > -1) then
        begin
          MarkDevice(FDeviceSelected,clGreen,True,False);
          MarkDevice(FDeviceSelected,clAqua,False,True);    
          MarkDevice(FDeviceSelected,clLime,False,False);
          Devs[FDeviceSelected].SelColor := clLime;
          Devs[FDeviceSelected].Selected := True;
        end Else
        begin
          for I := 0 to Length(Devs) -1 do
            begin
              Devs[I].Selected := False;
              Devs[I].SelColor := clLime;
            end;
        end;
      //Zeichne die Kabel:
      FDrawConnections;
      //Zeichne die Devices:
      for I := 0 to Length(Devs) -1 do
        Devs[I].Paint;
      MainFRM.BI.Picture.Bitmap.Assign(FImage); //Oder: MainFRM.BI.Canvas.Draw(0,0,FImage);
      MainFrm.Update;
    end;
  FRepainting := False;
end;

procedure TDeviceMngt.LoadFromPulse;
var
  I: Integer;
  RedrawMe: Boolean;
begin
  RedrawMe := ReloadPulseDevices(PD, FDefaultSink, FDefaultMic);
  if Length(Devs) > 0 then
    begin
      for I := 0 to Length(Devs) -1 do
        if (Devs[I] <> nil) and (Assigned(Devs[I])) then
          Devs[I].Free;
      SetLength(Devs,0);
    end;
  if Length(PD) > 0 then
    begin
      SetLength(Devs,Length(PD));
      If RedrawMe then
        begin
          if MainFrm.M3_DefSpeakers.Count > 0 then
            for I := MainFrm.M3_DefSpeakers.Count -1 downto 0 do
              MainFrm.M3_DefSpeakers.Delete(I);
          if MainFrm.M4_DefMicrofone.Count > 0 then
            for I := MainFrm.M4_DefMicrofone.Count -1 downto 0 do
              MainFrm.M4_DefMicrofone.Delete(I);
        end;
      for I := 0 to Length(PD) -1 do
        begin
          if (RedrawMe) and (PD[I].Typ = PDT_Speaker) then
            begin
              AddMenuItem(MainFrm.M3_DefSpeakers,PD[I].Name,@MainFrm.M3_ItemClick);
              MainFrm.M3_DefSpeakers.Items[MainFrm.M3_DefSpeakers.Count -1].Tag := I;
              if PD[I].Device_Name = FDefaultSink then
                MainFrm.M3_DefSpeakers.Items[MainFrm.M3_DefSpeakers.Count -1].Checked := True Else
                MainFrm.M3_DefSpeakers.Items[MainFrm.M3_DefSpeakers.Count -1].Checked := False;
            end;       
          if (RedrawMe) and ((PD[I].Typ = PDT_Microfone) or (PD[I].Typ = PDT_Monitor)) then
            begin
              AddMenuItem(MainFrm.M4_DefMicrofone,PD[I].Name,@MainFrm.M4_ItemClick);     
              MainFrm.M4_DefMicrofone.Items[MainFrm.M4_DefMicrofone.Count -1].Tag := I;
              if PD[I].Device_Name = FDefaultMic then
                MainFrm.M4_DefMicrofone.Items[MainFrm.M4_DefMicrofone.Count -1].Checked := True Else
                MainFrm.M4_DefMicrofone.Items[MainFrm.M4_DefMicrofone.Count -1].Checked := False;
            end;
          Devs[I] := TDevice.Create(PD[I]);
        end;
      If RedrawMe then
        Repaint;
    end;
end;

procedure TDeviceMngt.LoadFromFile(AFileName: String);
const
  Main = 'Devices';
var
  Ini: TIniFile;
  I, J: Integer;
  AD: TPulseDevices;
  S, T: String;
begin
  if (AFileName <> '') and (FileExists(AFileName)) then
    begin
      Ini := TIniFile.Create(AFileName);
      try
        SetLength(AD,Ini.ReadInteger(Main,'Count',0));        
        FDefaultSink := Ini.ReadString(Main,'DefaultSink','');
        FDefaultMic := Ini.ReadString(Main,'DefaultMic','');
        if Length(AD) > 0 then
          begin
            for I := 0 to Length(AD) -1 do
              begin
                AD[I].Typ := Ini.ReadInteger(Main,IntToStr(I)+'_Typ',PDT_UnknownDev);   
                AD[I].Ident := Ini.ReadString(Main,IntToStr(I)+'_Ident','');      
                AD[I].Name := Ini.ReadString(Main,IntToStr(I)+'_Name','');
                AD[I].Device_Name := Ini.ReadString(Main,IntToStr(I)+'_Device_Name','');
                AD[I].PlayingOn := Ini.ReadString(Main,IntToStr(I)+'_PlayingOn','');
                AD[I].RecordingFrom := Ini.ReadString(Main,IntToStr(I)+'_RecordingFrom','');  
                AD[I].Loop_In := Ini.ReadString(Main,IntToStr(I)+'_Loop_In','');
                AD[I].Loop_Out := Ini.ReadString(Main,IntToStr(I)+'_Loop_Out','');
                AD[I].Volume := Ini.ReadString(Main,IntToStr(I)+'_Volume','');
                AD[I].VSink := Ini.ReadBool(Main,IntToStr(I)+'_VSink',False);
                AD[I].Vol_Muted := Ini.ReadBool(Main,IntToStr(I)+'_VolMute',False);
              end;              
            //Lösche Virtuelle  Devices:
            RunCMD('pactl unload-module module-loopback');
            RunCMD('pactl unload-module module-null-sink');
            //Lade Virtuelle Sinks:
            for I := 0 to Length(AD) -1 do
              if (AD[I].Typ = PDT_Speaker) and (AD[I].VSink = True) then
                RunCmd('pactl load-module module-null-sink sink_name="'+AD[I].Device_Name+'" sink_properties=device.description="'+AD[I].Name+'"');  
            LoadFromPulse;
            //Lade Loopbacks:
            for I := 0 to Length(AD) -1 do
              if AD[I].Typ = PDT_Loop then
                begin
                  //AD[I].ID := '';
                  for J := 0 to Length(PD) -1 do
                    if AD[I].Loop_In = PD[J].Name then
                      begin
                        S := PD[J].ID;
                        break;
                      end;
                  for J := 0 to Length(PD) -1 do
                    if AD[I].Loop_Out = PD[J].Name then
                      begin
                        T := PD[J].ID;
                        break;
                      end;
                  RunCMD('pactl load-module module-loopback source='+S+' sink='+T+' latency_msec=10');
                end;
            //Link Devices [Mic, Monitor, Rec, Player]:
            if Length(PD) > 0 then
              for I := 0 to Length(AD) -1 do
                for J := 0 to Length(PD) -1 do
                  if (AD[I].Typ = PD[J].Typ) and (AD[I].VSink = PD[J].VSink) and
                     (AD[I].Ident = PD[J].Ident) and (AD[I].Typ <> PDT_Loop) then
                    begin
                      AD[I].ID := PD[J].ID;
                      SetVol(J,StrToIntDef(AD[I].Volume,-1));
                      SetMute(J,AD[I].Vol_Muted);
                      break;
                    end;
            //Add Cables from Player / Recorder:
            if Length(PD) > 0 then
              for I := 0 to Length(AD) -1 do
                if (AD[I].Typ = PDT_Player) then
                  RunCmd('pacmd move-sink-input '+AD[I].ID+' '+AD[I].PlayingOn) Else
                if (AD[I].Typ = PDT_Recorder) then
                  RunCmd('pacmd move-source-output '+AD[I].ID+' '+AD[I].RecordingFrom);
            //Set Defaults:
            if FDefaultSink <> '' then
              RunCMD('pacmd set-default-sink "'+FDefaultSink+'"');
            if FDefaultMic <> '' then         
              RunCMD('pacmd set-default-source "'+FDefaultMic+'"');
            LoadFromPulse;
          end;
      finally
        Ini.Free;
      end;
    end;
end;

procedure TDeviceMngt.SetMute(AID: Integer; Mute: Boolean);
var
  cmdstr: String;
begin
  if Mute then
    cmdstr := ' true' Else
    cmdstr := ' false';
  if (PD[AID].Typ = PDT_Player) then
    RunCmd('pacmd set-sink-input-mute '+PD[AID].ID+cmdstr) Else
  if (PD[AID].Typ = PDT_Speaker) then
    RunCmd('pacmd set-sink-mute '+PD[AID].ID+cmdstr) Else
  if (PD[AID].Typ = PDT_Microfone) or (Dev.PD[AID].Typ = PDT_Monitor) then
    RunCmd('pacmd set-source-mute '+PD[AID].ID+cmdstr) Else
  if (PD[AID].Typ = PDT_Recorder) then
    RunCmd('pacmd set-source-output-mute '+PD[AID].ID+cmdstr);
end;

procedure TDeviceMngt.SetVol(AID: Integer; AVol: Integer);
var
  IVol: Integer;
begin
  IVol := -1;
  if (AVol <> -1) and (AVol >= 0) and (AVol <= 100) then
    begin
      PD[AID].Volume := IntToStr(AVol);
      Devs[AID].D.Volume := IntToStr(AVol);
      IVol := Round((AVol / 100) * 65535);
      if (IVol > -1) and (IVol < 65536) then
        if (PD[AID].Typ = PDT_Player) then
          RunCMD('pacmd set-sink-input-volume '+PD[AID].ID+' '+IntToStr(IVol)) Else
        if (PD[AID].Typ = PDT_Speaker) then
          RunCMD('pacmd set-sink-volume '+PD[AID].ID+' '+IntToStr(IVol)) Else
        if (PD[AID].Typ = PDT_Microfone) or (Dev.PD[AID].Typ = PDT_Monitor) then
          RunCMD('pacmd set-source-volume '+PD[AID].Device_Name+' '+IntToStr(IVol)) Else
        if (PD[AID].Typ = PDT_Recorder) then
          RunCMD('pacmd set-source-output-volume '+PD[AID].ID+' '+IntToStr(IVol));
    end;
end;

function TDeviceMngt.SaveToFile(AFileName: String): Boolean;    
const
  Main = 'Devices';
var
  Ini: TIniFile;
  I, J: Integer;
  S: String;
begin
  Result := False;
  if AFileName <> '' then
    begin
      Ini := TIniFile.Create(AFileName);
      try
        Ini.WriteInteger(Main,'Count',Length(PD));
        Ini.WriteString(Main,'DefaultSink',FDefaultSink);    
        Ini.WriteString(Main,'DefaultMic',FDefaultMic);
        if Length(PD) > 0 then
          for I := 0 to Length(PD) -1 do
            begin
              Ini.WriteInteger(Main,IntToStr(I)+'_Typ',PD[I].Typ);        
              Ini.WriteString(Main,IntToStr(I)+'_Ident',PD[I].Ident);
              Ini.WriteString(Main,IntToStr(I)+'_Name',PD[I].Name);
              Ini.WriteString(Main,IntToStr(I)+'_Device_Name',PD[I].Device_Name);
              S := PD[I].PlayingOn;
              if (PD[I].Typ = PDT_Player) then
                for J := 0 to Length(PD) -1 do
                  if (PD[J].Typ = PDT_Speaker) and (PD[I].PlayingOn = PD[J].ID) then
                    begin
                      S := PD[J].Device_Name;
                      break;
                    end;
              Ini.WriteString(Main,IntToStr(I)+'_PlayingOn',S);         
              S := PD[I].RecordingFrom;
              if (PD[I].Typ = PDT_Recorder) then
                for J := 0 to Length(PD) -1 do
                  if ((PD[J].Typ = PDT_Monitor) or (PD[J].Typ = PDT_Microfone)) and (PD[I].RecordingFrom = PD[J].ID) then
                    begin
                      S := PD[J].Device_Name;
                      break;
                    end;
              Ini.WriteString(Main,IntToStr(I)+'_RecordingFrom',S);
              Ini.WriteString(Main,IntToStr(I)+'_Loop_In',PD[I].Loop_In);
              Ini.WriteString(Main,IntToStr(I)+'_Loop_Out',PD[I].Loop_Out);
              Ini.WriteBool(Main,IntToStr(I)+'_VSink',PD[I].VSink);     
              Ini.WriteString(Main,IntToStr(I)+'_Volume',PD[I].Volume);     
              Ini.WriteBool(Main,IntToStr(I)+'_VolMute',PD[I].Vol_Muted);
            end;
        Result := True;
      finally
        Ini.Free;
      end;
    end;
end;

function PointInRect(CheckP: TPoint; Lower, Higher: TPoint): Boolean;
var
  MinX, MaxX, MinY, MaxY: Integer;
begin
  if Lower.Y < Higher.Y then
    begin
      MinY := Lower.Y;
      MaxY := Higher.Y
    end Else
    begin
      MinY := Higher.Y;
      MaxY := Lower.Y;
    end;   
  if Lower.X < Higher.X then
    begin
      MinX := Lower.X;
      MaxX := Higher.X;
    end Else
    begin
      MinX := Higher.X;
      MaxX := Lower.X
    end;
  if (CheckP.X >= MinX) and (CheckP.X <= MaxX) and
     (CheckP.Y >= MinY) and (CheckP.Y <= MaxY) then
       Result := True Else
       Result := False;
end;

function TDeviceMngt.CheckSelectedDevice(AID: Integer; Mouse: TPoint): Boolean;
var
  P1, P2: TPoint;
begin     
  //Device:
  P1.X := Devs[AID].X;
  P1.Y := Devs[AID].Y;
  P2.X := Devs[AID].X + Devs[AID].Width;
  P2.Y := Devs[AID].Y + Devs[AID].Height;
  Result := PointInRect(Mouse,P1,P2);
end;

function TDeviceMngt.FCheckSelectedCable(ACable: TCable; Mouse: TPoint): Boolean;
var
  P1, P2: TPoint;
begin
  Result := False;
  P1 := ACable.Lower;
  P2 := ACable.Higher;
  //1. Kabelstück Device -->
  P1.X := ACable.Lower.X;
  P1.Y := ACable.Lower.Y -3;
  P2.X := ACable.Lower.X+( (ACable.Higher.X - ACable.Lower.X) div 2)+22;
  P2.Y := ACable.Lower.Y +3;
  Result := PointInRect(Mouse,P1,P2);
  if Result = True then exit;
  // 2. Kabelstück (wird ignoriert)
  //3. Kabelstück: --> Device
  P1.X := ACable.Higher.X;
  P1.Y := ACable.Higher.Y -3;
  P2.X := ACable.Higher.X-22;
  P2.Y := ACable.Higher.Y +3;
  Result := PointInRect(Mouse,P1,P2); 
  if Result = True then exit;
  Result := CheckSelectedDevice(ACable.Input, Mouse);
  if Result = True then exit;                         
  Result := CheckSelectedDevice(ACable.Output, Mouse);
end;

procedure TDeviceMngt.InitCableMove(P: TPoint);
var
  I: Integer;
begin
  If (Length(Devs) > 0) then
    begin
      for I := 0 to Length(Devs) -1 do
        if CheckSelectedDevice(I,P) then
          begin
            FStartMoveP := P;
            FDeviceSelected := I;
          end Else
            Devs[I].Selected := False;
      Repaint;
    end;
end;

procedure TDeviceMngt.DoCableMove(P: TPoint);
var
  I, J, K: Integer;
  SelDev: Byte;
begin
  If (FDeviceSelected > -1) and (P.X <> FLastMoveP.X) and (P.Y <> FLastMoveP.Y) then
    begin
      FLastMoveP := P;
      for I := 0 to Length(Devs) -1 do
        if CheckSelectedDevice(I,P) then
          begin
            J := PD[FDeviceSelected].Typ;
            If PD[FDeviceSelected].Typ = PDT_Loop then
              If PD[I].Typ = PDT_Speaker then
                J := PDT_LoopSpk Else
                J := PDT_LoopRec;
            K := PD[I].Typ;
            If PD[I].Typ = PDT_Loop then
              If PD[FDeviceSelected].Typ = PDT_Speaker then
                K := PDT_LoopSpk Else
                K := PDT_LoopRec;
            case J of
              PDT_Player, PDT_LoopSpk: if K = PDT_Speaker then
                            SelDev := 1 Else
                            SelDev := 2;    
              PDT_Speaker: if (K = PDT_Player) or (K = PDT_LoopSpk) then
                            SelDev := 1 Else
                            SelDev := 2;     
              PDT_Microfone, PDT_Monitor: if (K = PDT_Recorder) or (K = PDT_LoopRec) then
                            SelDev := 1 Else
                            SelDev := 2;
              PDT_Recorder, PDT_LoopRec: if (K = PDT_Microfone) or (K = PDT_Monitor) then
                              SelDev := 1 Else
                              SelDev := 2;
              Else          SelDev := 0;
            end;
            case SelDev of
              0: begin  
                   Devs[I].SelColor := clLime;
                   Devs[I].Selected := False;
                 end;     
              1: begin
                   Devs[I].SelColor := clLime;
                   Devs[I].Selected := True;
                 end;
              2: begin
                   Devs[I].SelColor := clRed;
                   Devs[I].Selected := True;
                 end;
            end;
          end Else
          begin
            Devs[I].SelColor := clLime;
            Devs[I].Selected := False;
          end;
      Repaint;
      MainFRM.PB_NewLine.Canvas.Clear;
      FDrawSingleLine(FStartMoveP,P,clLime);
    end;
end;

procedure TDeviceMngt.EndCableMove(P: TPoint);
var
  I, J, K: Integer;
  S: String;
  Applied: Boolean;
begin
  If FDeviceSelected > -1 then
      begin
        Applied := False;
        for I := 0 to Length(Devs) -1 do
          if CheckSelectedDevice(I,P) then
            begin
              J := PD[FDeviceSelected].Typ;
              If PD[FDeviceSelected].Typ = PDT_Loop then
                If PD[I].Typ = PDT_Speaker then
                  J := PDT_LoopSpk Else
                  J := PDT_LoopRec;    
              K := PD[I].Typ;
              If PD[I].Typ = PDT_Loop then
                If PD[FDeviceSelected].Typ = PDT_Speaker then
                  K := PDT_LoopSpk Else
                  K := PDT_LoopRec;
              case J of
                PDT_Player, PDT_LoopSpk:
                             if K = PDT_Speaker then
                              begin
                                //Anwendung --> Speaker
                                RunCmd('pacmd move-sink-input '+PD[FDeviceSelected].ID+' '+PD[I].Device_Name);
                                Applied := True;
                              end;
                PDT_Speaker: if (K = PDT_Player) or (K = PDT_LoopSpk) then
                              begin
                                //Speaker --> Anmwendung         
                                RunCmd('pacmd move-sink-input '+PD[I].ID+' '+PD[FDeviceSelected].Device_Name);
                                Applied := True;
                              end;
                PDT_Microfone, PDT_Monitor: if (K = PDT_Recorder) or (K = PDT_LoopRec) then
                              begin
                                //Mic OR Monitor --> Recorder or Loop (Rec)
                                If K = PDT_Recorder then
                                  S := PD[I].ID Else
                                  S := PD[I].MicID;
                                RunCmd('pacmd move-source-output '+S+' '+PD[FDeviceSelected].Device_Name);
                                Applied := True;
                              end;
                PDT_Recorder, PDT_LoopRec: if (K = PDT_Microfone) or (K = PDT_Monitor) then
                              begin
                                //Recorder or Loop (Rec) --> MIC OR Monitor
                                If J = PDT_Recorder then
                                  S := PD[FDeviceSelected].ID Else
                                  S := PD[FDeviceSelected].MicID;
                                RunCmd('pacmd move-source-output '+S+' '+PD[I].Device_Name);
                                Applied := True;
                              end;
              end;
            break;
          end;
        FDeviceSelected := -1;
        If Applied = True then
          LoadFromPulse Else
          Repaint;
      end;
end;

destructor TDeviceMngt.Free;
var
  I: Integer;
begin         
  if Length(Devs) > 0 then
    begin
      for I := 0 to Length(Devs) -1 do
        if (Devs[I] <> nil) and (Assigned(Devs[I])) then
          Devs[I].Free;
      SetLength(Devs,0);
    end;
  SetLength(PD,0);
  FImage.Free;
end;

{TDevice}    

constructor TDevice.Create(AD: TPulseDevice);
begin
  inherited Create;
  SelColor := clLime;
  FOldD.Name := '0';
  D := AD;
  Selected := False;
  X := 20;
  Y := 50;
end;

procedure TDevice.Paint;
var
  Bitmap: TBitmap;
  TX: Integer;
  Col: Integer;
begin
  If (FOldD.Name <> D.Name) or (FOldD.Volume <> D.Volume) then
    begin
      FSetCaption;
      GetWidth;
      GetHeight;
      MainFrm.CalcLab.Caption := '';
      FOldD := D;
    end Else
      FOldD := D;
  Bitmap := TBitmap.Create;
  try
    Bitmap.Height := FHeight;
    Bitmap.Width := FWidth;

    Bitmap.Canvas.Brush.Color := clBackgr;
    Bitmap.Canvas.FillRect(0,0,FWidth, FHeight);

    //Bitmap.Canvas.Pen.Color := clLines;
    //Bitmap.Canvas.Rectangle(0, 0, FWidth, FHeight);
               
    Bitmap.Canvas.Font.Color := clLines;
    if FSelected then
      Col := SelColor Else
    if D.Vol_Muted then
      begin
        Col := clGray;          
        Bitmap.Canvas.Font.Color := clGray;
      end Else
        Col := clLines;
    Bitmap.Canvas.Pen.Color := Col;
    Bitmap.Canvas.Pen.Width := 3;
    if (PDT_Speaker and D.Typ <> 0) or (PDT_Microfone and D.Typ <> 0) or (PDT_Monitor and D.Typ <> 0) then
      begin
        Bitmap.Canvas.Rectangle(10,1,FWidth-1,FHeight-1);
        TX := 15;
      end Else
      begin
        Bitmap.Canvas.Rectangle(1,1,FWidth-10,FHeight-1);
        TX := 5;
      end;
    //Eingangspfeil (offen) Links / Oben (Audioeingabe / Lautsprecher):
    if (PDT_Speaker and D.Typ <> 0) then
      begin
        Bitmap.Canvas.Line(1,1,10,6);
        Bitmap.Canvas.Line(1,11,10,6);
      end;          
    //Ausgangspfeil (offen) Rechts / Oben (Audioausgabe / Player):
    if (PDT_Player and D.Typ <> 0) or (PDT_Loop and D.Typ <> 0) then
      begin
        Bitmap.Canvas.Line(Fwidth -11,4,Fwidth -11,10);
        Bitmap.Canvas.Line(Fwidth -10,1,Fwidth,6);
        Bitmap.Canvas.Line(Fwidth -10,11,Fwidth,6);
      end;
    //Eingangspfeil (geschlossen) Rechts / Unten (Audioeingabe / Recorder):
    if (PDT_Recorder and D.Typ <> 0) or (PDT_Loop and D.Typ <> 0) then
      begin
        Bitmap.Canvas.Line(Fwidth,Fheight-1,Fwidth-10,Fheight-6);
        Bitmap.Canvas.Line(Fwidth,Fheight-11,Fwidth-10,Fheight-6);
        Bitmap.Canvas.Line(Fwidth-1,Fheight-1,Fwidth-1,Fheight-11);
      end;
    //Ausgangspfeil Unten (Audioausgabe / Player):
    if (PDT_Microfone and D.Typ <> 0) or (PDT_Monitor and D.Typ <> 0) then
      begin
        Bitmap.Canvas.Line(10,Fheight-1,0,Fheight-6);
        Bitmap.Canvas.Line(10,Fheight-11,0,Fheight-6);
      end;
    if FSecCaption <> '' then
      begin  
        Bitmap.Canvas.TextOut(TX,3,FMainCaption);
        Bitmap.Canvas.TextOut(TX,FTextHeight+3,FSecCaption);
      end else
        Bitmap.Canvas.TextOut(TX,3,FMainCaption);
    Dev.FImage.Canvas.Draw(X, Y, Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure TDevice.SetFX(AX: Integer);
begin       
  if AX < 0 then
    FX := 0 Else
  if AX > MainFRM.ClientWidth - FWidth then
    FX := MainFRM.ClientWidth - FWidth Else
    FX := AX;
end;

procedure TDevice.SetFY(AY: Integer);
begin
  if AY < 0 then
    FY := 0 Else
    FY := AY;
end;

procedure TDevice.FSetCaption;
const
  VSinkStr = '[V] ';
var
  S, T, V: String;
begin
  //Setze die "Caption":
  If D.VSink = True then
    S := VSinkStr Else
    S := '';
  case D.Typ of
    PDT_Speaker: S := S+'Speaker';
    PDT_Player: if (S <> '') and (S <> VSinkStr) then
                  S := S + ' / Player' Else
                  S := S + 'Player';
    PDT_Recorder: if (S <> '') and (S <> VSinkStr) then
                    S := S + ' / Rec' Else
                    S := S +'Rec';
    PDT_Microfone, PDT_Monitor: begin
                                  if PDT_Monitor and D.Typ <> 0 then
                                    T := 'Monitor' Else
                                    T := 'Mic';
                                  if (S <> '') and (S <> VSinkStr) then
                                    S := S + ' / '+T Else
                                    S := S + T;
                                end;
    end;
  FSecCaption := '';
  If D.Volume <> '' then
    V := ' ['+D.Volume+'%] ' Else
    V := '';
  if (D.Typ = PDT_Loop) then
    FMainCaption := 'LOOP' Else
  if (D.Typ = PDT_Monitor) then
    FMainCaption := S+V Else
    begin
      FMainCaption := S+V+':';
      FSecCaption := D.Name;
    end;
end;

function TDevice.GetWidth: Integer;
var
  L1, L2: Integer;
begin
  if FMainCaption = '' then
    FSetCaption;
  MainFrm.CalcLab.Caption := FMainCaption;
  L1 := 25 + MainFrm.CalcLab.Canvas.TextWidth(FMainCaption);
  MainFrm.CalcLab.Caption := FSecCaption;
  L2 := 25 + MainFrm.CalcLab.Canvas.TextWidth(FSecCaption);
  MainFrm.CalcLab.Caption := '';
  If L1 > L2 then
    L2 := L1;
  Result := L2;
  FWidth := Result;
end;

function TDevice.GetHeight: Integer;
var
  H1, H2: Integer;
begin            
  if FMainCaption = '' then
    FSetCaption;
  MainFrm.CalcLab.Caption := FMainCaption;
  H1 := MainFrm.CalcLab.Canvas.TextHeight(FMainCaption);
  MainFrm.CalcLab.Caption := FSecCaption;
  H2 := MainFrm.CalcLab.Canvas.TextHeight(FSecCaption);
  If H1 > H2 then
    H2 := H1;
  FTextHeight := H2;
  if FSecCaption = '' then
    Result := FTextHeight+6 Else
    Result := (FTextHeight*2) +6;
  FHeight := Result;
end;

{ TMainFRM }

procedure TMainFRM.M01_ExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainFRM.M01_LoadClick(Sender: TObject);
var
  OD: TOpenDialog;
begin
  OD := TOpenDialog.Create(nil);
  try
    OD.Filter := 'Audio-Config|*.pacm';
    If (OD.Execute) and (OD.FileName <> '') then
      begin
        AddLoadMenu(OD.FileName);
        Dev.LoadFromFile(OD.FileName);
      end;
  finally
    OD.Free;
  end;
end;

procedure TMainFRM.M3_ItemClick(Sender: TObject);
var
  I: Integer;
begin
  if Length(Dev.PD) > 0 then
    for I := 0 to Length(Dev.PD) -1 do
      if Dev.PD[I].Name = (Sender as TMenuItem).Caption then
        begin
          RunCMD('pacmd set-default-sink "'+Dev.PD[(Sender as TMenuItem).Tag].Device_Name+'"');
          Dev.LoadFromPulse;
          break;
        end;
end;

procedure TMainFRM.M4_ItemClick(Sender: TObject);     
var
  I: Integer;
begin      
  if Length(Dev.PD) > 0 then
    for I := 0 to Length(Dev.PD) -1 do
      if Dev.PD[I].Name = (Sender as TMenuItem).Caption then
        begin
          RunCMD('pacmd set-default-source "'+Dev.PD[(Sender as TMenuItem).Tag].Device_Name+'"');
          Dev.LoadFromPulse;
          break;
        end;
end;

procedure TMainFRM.FormResize(Sender: TObject);
begin
  //Nicht notwenig.. Wird bei Repaint der Grafik erledigt.. Ähh doch notwendig... Zeichnen wird nicht mehr durch On Paint gemacht
  if (Dev <> nil) and (Assigned(Dev)) then
    Dev.Repaint;
end;

procedure TMainFRM.FormClose(Sender: TObject; var CloseAction: TCloseAction);
const
  Ini_Main = 'Main';    
  Ini_File = 'Files';
var
  Ini: TIniFile;
  I: Byte;
begin
  if (Dev <> nil) and Assigned(Dev) then
    Dev.Free;
  Ini := TIniFile.Create(GetAppConfigFile(False));
  try
    Ini.WriteBool(Ini_Main,'Maximized',WindowState = wsMaximized);
    If MainFrm.WindowState = wsMaximized then
      WindowState := wsNormal;
    Ini.WriteInteger(Ini_Main,'XPos',MainFrm.Left);
    Ini.WriteInteger(Ini_Main,'YPos',MainFrm.Top);
    Ini.WriteInteger(Ini_Main,'Width',MainFrm.Width);
    Ini.WriteInteger(Ini_Main,'Height',MainFrm.Height);
    Ini.WriteBool(Ini_Main,'DarkMode',M01_OP_DarkM.Checked);
    Ini.WriteBool(Ini_Main,'AutoReload',M01_OP_AutoReload.Checked);
    Ini.WriteBool(Ini_Main,'OnTop',M01_OP_OnTop.Checked);
    Ini.WriteInteger(Ini_File,'Count',RecentFiles.Count);
    If RecentFiles.Count > 0 then
      for I := 0 to RecentFiles.Count -1 do
        Ini.WriteString(Ini_File,IntToStr(I)+'_File',RecentFiles[I]);
  finally
    Ini.Free;
  end;
  RecentFiles.Free;
end;

procedure TMainFRM.AutoTTimer(Sender: TObject);
begin
  If AutoT_Cnt >= 10 then
    begin
      Dev.LoadFromPulse;
      SBMouseDown(Sender,mbLeft,[ssShift],0,0);
      SBMouseUp(Sender,mbLeft,[ssShift],0,0);
      AutoT_Cnt := 0;
    end Else
  Inc(AutoT_Cnt);
end;

function AppName: String;
begin
  Result:= 'PA_CableManager';
end;

procedure TMainFRM.FormShow(Sender: TObject);
const
  Ini_Main = 'Main';
  Ini_File = 'Files';
var
  Ini: TIniFile;
  S: String;
  FCnt, I: Byte;
begin
  RecentFiles := TStringList.Create;
  OnGetApplicationName := @AppName;
  Ini := TIniFile.Create(GetAppConfigFile(False));
  try
    MainFrm.Left := Ini.ReadInteger(Ini_Main,'XPos',MainFrm.Left);
    MainFrm.Top := Ini.ReadInteger(Ini_Main,'YPos',MainFrm.Top);
    MainFrm.Width := Ini.ReadInteger(Ini_Main,'Width',MainFrm.Width);
    MainFrm.Height := Ini.ReadInteger(Ini_Main,'Height',MainFrm.Height);
    If Ini.ReadBool(Ini_Main,'Maximized',False) then
      MainFrm.WindowState := wsMaximized;
    M01_OP_DarkM.Checked := Ini.ReadBool(Ini_Main,'DarkMode',False);     
    SetDarkMode(M01_OP_DarkM.Checked);
    M01_OP_AutoReload.Checked := Ini.ReadBool(Ini_Main,'AutoReload',False);   
    AutoT_Cnt := 0;
    AutoT.Enabled := M01_OP_AutoReload.Checked;
    M5_Reload.Visible := not M01_OP_AutoReload.Checked;
    M01_OP_OnTop.Checked := Ini.ReadBool(Ini_Main,'OnTop',False);      
    If M01_OP_OnTop.Checked then
      FormStyle:=fsSystemStayOnTop Else
      FormStyle:=fsNormal;
    FCnt := Ini.ReadInteger(Ini_File,'Count',0);
    If FCnt > 0 then
      for I := 0 to FCnt -1 do
        begin
          S := Ini.ReadString(Ini_File,IntToStr(I)+'_File','');
          If (S <> '') and (FileExists(S)) then
            RecentFiles.Add(S);
        end;
    UpdateLoadMenu;
  finally
    Ini.Free;
  end;
  Dev := TDeviceMngt.Create;
  Dev.LoadFromPulse;
end;

procedure TMainFRM.UpdateLoadMenu;
var
  I: Byte;
begin
  If M01_LoadRecent.Count > 0 then
    for I := MainFrm.M01_LoadRecent.Count -1 downto 0 do
      MainFrm.M01_LoadRecent.Delete(I);
  if RecentFiles.Count > 0 then
    for I := 0 to RecentFiles.Count -1 do
      AddMenuItem(M01_LoadRecent,ExtractFileName(RecentFiles[I]),@M01_LoadRecentClick);
end;

procedure TMainFRM.AddLoadMenu(AFile: String);
var
  I: Byte;
begin
  If (AFile <> '') and (FileExists(AFile)) then
    begin
      RecentFiles.Insert(0,AFile);
      if RecentFiles.Count > 1 then
        for I := 1 to RecentFiles.Count -1 do
          if ExtractFileName(AFile) = ExtractFileName(RecentFiles[I]) then
            begin
              RecentFiles.Delete(I);
              break;
            end;
      if RecentFiles.Count > 5 then
        repeat
          RecentFiles.Delete(RecentFiles.Count -1);
        until RecentFiles.Count <= 5;
      UpdateLoadMenu;
    end;
end;

procedure TMainFRM.M01_LoadRecentClick(Sender: TObject);
var
  I: Byte;
begin
  If (RecentFiles.Count > 0) and ((Sender As TMenuItem).Caption <> '') then
    for I := 0 to RecentFiles.Count -1 do
      If ((Sender As TMenuItem).Caption = ExtractFileName(RecentFiles[I])) and (FileExists(RecentFiles[I])) then
        begin
          Dev.LoadFromFile(RecentFiles[I]);
          break;
        end;
end;

procedure TMainFRM.SetDarkMode(IsDark: Boolean);
begin
  if IsDark then
    begin
      clLines := clWhite;
      clBackgr := clBlack;
    end Else
    begin
      clLines := clBlack;
      clBackgr := clWhite;
    end;
end;

procedure TMainFRM.M01_NewClick(Sender: TObject);
begin

end;

procedure TMainFRM.M01_OP_AutoReloadClick(Sender: TObject);
begin
  M01_OP_AutoReload.Checked := not M01_OP_AutoReload.Checked;
  AutoT_Cnt := 0;
  AutoT.Enabled := M01_OP_AutoReload.Checked;
  M5_Reload.Visible := not M01_OP_AutoReload.Checked;
end;

procedure TMainFRM.M01_OP_OnTopClick(Sender: TObject);
begin
  M01_OP_OnTop.Checked := not M01_OP_OnTop.Checked;
  If M01_OP_OnTop.Checked then
    FormStyle:=fsSystemStayOnTop Else
    FormStyle:=fsNormal;
end;

procedure TMainFRM.M01_SaveAsClick(Sender: TObject);
var
  SL: TSaveDialog;
  S: String;
begin
  SL := TSaveDialog.Create(nil);
  try
    SL.Filter := 'Audio-Config|*.pacm';
    If (SL.Execute) and (SL.FileName <> '') then
      begin
        if (FileExists(SL.FileName)) and (QuestionDlg('Replace','Replace existing file?',mtCustom,[mrYes,mrNo],0) = mrNo) then
          exit;
        S := SL.FileName;
        If AnsiLowerCase(ExtractFileExt(SL.FileName)) <> '.pacm' then
          S := S + '.pacm';
        If Dev.SaveToFile(S) then
          begin
            AddLoadMenu(S);
            ShowMessage('Settings saved.');
          end else
          ShowMessage('Settings could not be saved.')
      end;
  finally
    SL.Free;
  end;
end;

procedure TMainFRM.M02_AddLoopClick(Sender: TObject);
begin
  RunCMD('pactl load-module module-loopback latency_msec=10');
  Dev.LoadFromPulse;
end;

procedure TMainFRM.M02_AddVSinkClick(Sender: TObject);
  function RandomString(strlength: integer): string;
  var
    temp : integer;
  begin
    randomize;
    Result := '';
    repeat
      temp := random(122); //ggf. erhöhen
      if temp in [48..57{0-1}, 65..90{A-Z}, 97..122{a-z}] then
      //Kann um beliebige ASCII-Zeichen erweitert werden,
      //ggf. den Wert in Random hochsetzen
        result := result + Chr(temp);
    until length(result) = strlength;
  end;
var
  S: String;
begin
  S := InputBox('New virtual Sink:','Name','');
  If StringReplace(S,' ','',[rfreplaceall]) <> '' then
    begin
      RunCmd('pactl load-module module-null-sink sink_name="VSINK_'+RandomString(10)+'_'+S+'" sink_properties=device.description="'+S+'"');
      Dev.LoadFromPulse;
    end;
end;

procedure TMainFRM.M02_Dell_AllClick(Sender: TObject);
begin                            
  RunCMD('pactl unload-module module-loopback');
  RunCMD('pactl unload-module module-null-sink');
  Dev.LoadFromPulse;
end;

procedure TMainFRM.M02_Del_LoopsClick(Sender: TObject);
begin
  RunCMD('pactl unload-module module-loopback');
  Dev.LoadFromPulse;
end;

procedure TMainFRM.M02_Del_SinksClick(Sender: TObject);
begin          
  RunCMD('pactl unload-module module-null-sink');
  Dev.LoadFromPulse;
end;

procedure TMainFRM.M5_ReloadClick(Sender: TObject);
begin
  Dev.LoadFromPulse;
  //Only for debugging:
  //Clipboard.AsText := RunCMD('pactl list');
end;

procedure TMainFRM.M01_OP_DarkMClick(Sender: TObject);
begin
  M01_OP_DarkM.Checked := not M01_OP_DarkM.Checked;
  SetDarkMode(M01_OP_DarkM.Checked);
  Dev.Repaint;
end;

procedure TMainFRM.RC_DelClick(Sender: TObject);
begin
  RunCMD('pactl unload-module '+Dev.PD[RC_Dev].Module_ID);
  Dev.LoadFromPulse;
end;

procedure TMainFRM.SBMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if M01_OP_AutoReload.Checked then
    begin
      AutoT_Cnt := 0;
      AutoT.Enabled := False;
    end;
  if mbLeft = Button then
    begin
      P.X := X;
      P.Y := Y;
      Dev.InitCableMove(P);
    end;
end;    

procedure TMainFRM.PB_NewLineMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  P: TPoint;
begin
  P.X := X;
  P.Y := Y;
  Dev.DoCableMove(P);
end;

procedure TMainFRM.PB_NewLineMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  I: Integer;
  IVol: Integer;
begin
  if Length(Dev.PD) > 0 then
    for I := 0 to Length(Dev.PD) -1 do
      if Dev.CheckSelectedDevice(I,MousePos) then
        begin
          Handled := True;
          IVol := StrToIntDef(Dev.PD[I].Volume,-1);
          if (IVol <> -1) and (IVol > 0) then
            begin
              IVol := IVol -1;
              Dev.SetVol(I,IVol);
              Dev.RePaint;
            end;
          break;
        end;
end;

procedure TMainFRM.PB_NewLineMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  I: Integer;
  IVol: Integer;
begin
  if Length(Dev.PD) > 0 then
    for I := 0 to Length(Dev.PD) -1 do
      if Dev.CheckSelectedDevice(I,MousePos) then
        begin
          Handled := True;
          IVol := StrToIntDef(Dev.PD[I].Volume,-1);
          if (IVol <> -1) and (IVol < 100) then
            begin
              IVol := IVol +1;
              Dev.SetVol(I,IVol); 
              Dev.Repaint;
            end;  
          break;
        end;
end;

procedure TMainFRM.SBMouseLeave(Sender: TObject);
var
  P: TPoint;
begin
  P.X := 0;
  P.Y := 0;
  Dev.EndCableMove(P);   
  if M01_OP_AutoReload.Checked then
    begin
      AutoT_Cnt := 0;
      AutoT.Enabled := True;
    end;
end;

procedure TMainFRM.SBMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  I: Integer;
begin
  if mbLeft = Button then
    begin
      P.X := X;
      P.Y := Y;
      Dev.EndCableMove(P);
    end else
  if mbRight = Button then
    begin   
      P.X := X;
      P.Y := Y;
      if Length(Dev.PD) > 0 then
        for I := 0 to Length(Dev.PD) -1 do
          if Dev.CheckSelectedDevice(I,P) then
            begin
              if (Dev.PD[I].Typ = PDT_Loop) or
                 (Dev.PD[I].VSink = True) then
                 begin
                   RC_Dev := I;
                   RC.PopUp;
                 end;
              break;
            end;
    end;
  if mbMiddle = Button then
    begin
      P.X := X;
      P.Y := Y;
      if Length(Dev.PD) > 0 then
        for I := 0 to Length(Dev.PD) -1 do
          if Dev.CheckSelectedDevice(I,P) then
            begin
              Dev.SetMute(I,not Dev.PD[I].Vol_Muted);
              Dev.LoadFromPulse;
              break;
            end;
    end;  
  if M01_OP_AutoReload.Checked then
    begin
      AutoT_Cnt := 0;
      AutoT.Enabled := True;
    end;
end;

end.
