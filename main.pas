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
  MAX_CAPSIZE = 60;
  MAX_VOL = 153;

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
    DefaultSink: Boolean;     
    DefaultSource: Boolean;
  end;

type
  TPulseProp = record
    Name: String; //Gut Lesbarer Name
    PulseName: String; //Pulse DeviceName
    Avail: Boolean;
  end;

type
  TPulseCard = record
    Name: String;
    PulseName: String;
    Props: Array of TPulseProp;
    CurrentProp: Int16; //= Array Element
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
type TPulseCards = Array of TPulseCard;

type
  TDevice = class
  private
    FMainCaption, FSecCaption: String;
    FTextHeight: Integer;
    FWidth, FHeight: Integer;
    FX, FY: Integer;
    FSelected: Boolean;
    FDefaultSink: Boolean;  
    FDefaultSource: Boolean;
    FOldD: TPulseDevice;
    procedure FSetCaption;
    procedure FSetDefaultSink(ADefault: Boolean);  
    procedure FSetDefaultSource(ADefault: Boolean);
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
    property DefaultSink: Boolean read FDefaultSink write FSetDefaultSink;
    property DefaultSource: Boolean read FDefaultSource write FSetDefaultSource;
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
      procedure FGetLinePoint(ADev: TDevice; var PlyP: TPoint; var RecP: TPoint; const RecUnderPl: Boolean = True);
      procedure FDrawSingleLine(Lower, Higher: TPoint; AColor: Integer);
      procedure FDrawOneConnection(Lower, Higher: TPoint; AColor: Integer; OneIsLoop: Boolean; ToLoop: Boolean);
      procedure FDrawDeviceLines(var Con: TCable);
      procedure FCalcConnections;
      procedure FDrawConnections;
    public
      Devs: Array of TDevice;
      PD: TPulseDevices;
      PC: TPulseCards;
      property DefaultSink: String read FDefaultSink;
      property DefaultMic: String read FDefaultMic;
      constructor Create;
      function CheckSelectedDevice(AID: Integer; Mouse: TPoint): Boolean;
      procedure Repaint;
      procedure LoadFromPulse(const ForceFullReload: Boolean = False);
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
    M6_Reload: TMenuItem;
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
    M5_CardConfig: TMenuItem;
    M01_OP_HideCards: TMenuItem;
    M01_OP_Mode_Pipewire: TMenuItem;
    M01_OP_LoopDelay: TMenuItem;
    RC_SetDef: TMenuItem;
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
    procedure M01_OP_AutoReloadClick(Sender: TObject);
    procedure M01_OP_HideCardsClick(Sender: TObject);
    procedure M01_OP_LoopDelayClick(Sender: TObject);
    procedure M01_OP_Mode_PipewireClick(Sender: TObject);
    procedure M01_OP_OnTopClick(Sender: TObject);
    procedure M01_SaveAsClick(Sender: TObject);
    procedure M02_AddLoopClick(Sender: TObject);
    procedure M02_AddVSinkClick(Sender: TObject);
    procedure M02_Dell_AllClick(Sender: TObject);
    procedure M02_Del_LoopsClick(Sender: TObject);
    procedure M02_Del_SinksClick(Sender: TObject);
    procedure M6_ReloadClick(Sender: TObject);
    procedure M01_OP_DarkMClick(Sender: TObject);
    procedure PB_NewLineMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PB_NewLineMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure PB_NewLineMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure RC_DelClick(Sender: TObject);
    procedure RC_SetDefClick(Sender: TObject);
    procedure SBMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SBMouseLeave(Sender: TObject);
    procedure SBMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FCurMouseP: TPoint;
    AutoT_Cnt: Byte;
    RC_Dev: Integer;
    RecentFiles: TStringList;
    procedure SetDarkMode(IsDark: Boolean);
    procedure UpdateLoadMenu;
    procedure AddLoadMenu(AFile: String);
  public
    procedure M3_ItemClick(Sender: TObject);
    procedure M4_ItemClick(Sender: TObject);
    procedure M5_ItemClick(Sender: TObject);
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
  Mode_Pipewire: Boolean;
  LoopDelay: Integer;

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
    //WriteLn(Cmd);         //DEBUG
    GetProcess(Cmd,TS);
    Result := TS.Text;
    //WriteLn(TS.Text);         //DEBUG
  finally
    TS.Free;
  end;
end;

function ReloadPulseDevices(var PD: TPulseDevices; var PC: TPulseCards; var DefaultSink: String; var DefaultMic: String): Boolean;
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
      AD.DefaultSink := False;  
      AD.DefaultSource := False;
    end;
  procedure ResetPulseCard(var AC: TPulseCard);
    begin
      AC.Name := '';    
      AC.PulseName := '';
      SetLength(AC.Props,0);
      AC.CurrentProp := -1;
    end;
  function CopyFromPos(Source: String; const FromStr: String; const ToStr: String = ''): String;
    var
      I: Integer;
    begin
      I := Pos(FromStr,Source)+Length(FromStr);
      If ToStr = '' then
        Result := Copy(Source,I,Length(Source)-I+1) Else
        Result := Copy(Source,I,Pos(ToStr, Source,I+2)-I);
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
  function IsNewCard(StrToCheck: String): Boolean;
    begin
      Result := False;
      if (Pos('#',StrToCheck) > 0) and
         (Pos('Card ',StrToCheck) > 0) then
           Result := True;
    end;
var
  SL: TStringList;
  I, J, K, C: Integer;
  ND: TPulseDevice;
  Comp: String;
  AStr: String;
  ProfileSub: Boolean;
begin
  SetLength(PD,0);     
  SetLength(PC,0);
  SL := tStringList.Create;
  try
    //Get Default Devices:
    If Mode_Pipewire = true then
      begin
        DefaultSink := '';
        DefaultMic := '';
        GetProcess('pactl get-default-sink',SL);
        If SL.Count > 0 then
          DefaultSink := SL[0];
        SL.Clear;
        GetProcess('pactl get-default-source',SL);
        If SL.Count > 0 then
          DefaultMic := SL[0];
      end else
      begin
        GetProcess('pacmd stat',SL);
        If SL.Count > 0 then
          for I := 0 to SL.Count -1 do
            if (DefaultSink <> '') and (DefaultMic <> '') then
              break Else
            if Pos('Default sink name: ',SL[I]) > 0 then
              DefaultSink := CopyFromPos(SL[I],'Default sink name: ') Else
            if Pos('Default source name: ',SL[I]) > 0 then
              DefaultMic := CopyFromPos(SL[I],'Default source name: ');
      end;
    Comp := DefaultSink;
    Comp := Comp + DefaultMic;
    //Get Device Data:
    SL.Clear;
    GetProcess('pactl list',SL);
    I := 0;
    If SL.Count > 0 then
      repeat
        if IsNewCard(SL[I]) = True then
          begin
            SetLength(PC,Length(PC) +1);
            C := Length(PC) -1;
            ResetPulseCard(PC[C]);
            ProfileSub := False;
            for J := I to SL.Count -1 do
              begin
                if ProfileSub = true then
                  begin
                    if Pos('Active Profile: ',SL[I]) > 0 then
                      begin
                        AStr := CopyFromPos(SL[I],'Active Profile: ');
                        if Length(PC[C].Props) > 0 then
                          for K := 0 to Length(PC[C].Props) -1 do
                            if AStr = PC[C].Props[K].PulseName then
                              begin
                                PC[C].CurrentProp := K;
                                Comp := Comp + IntToStr(K);
                                break;
                              end;
                        ProfileSub := False;
                        break;
                      end Else
                      begin
                        SetLength(PC[C].Props,Length(PC[C].Props)+1);
                        K := Length(PC[C].Props) -1;
                        PC[C].Props[K].PulseName := CopyFromPos(SL[I],#9#9,': ');
                        Comp := Comp + PC[C].Props[K].PulseName;
                        PC[C].Props[K].Name := CopyFromPos(SL[I],PC[C].Props[K].PulseName + ': ',' (sinks:');
                        PC[C].Props[K].Avail := False;
                        if (Pos('available: no)',SL[I]) <= 0) and (Pos('available: yes)',SL[I]) > 0) then
                          begin
                            PC[C].Props[K].Avail := True;
                            Comp := Comp + 'T';
                          end;
                      end;
                  end Else
                if Pos('Name: ',SL[I]) > 0 then
                  begin
                    PC[C].PulseName := CopyFromPos(SL[I],'Name: ');
                    Comp := Comp + PC[C].PulseName;
                  end Else
                if Pos('alsa.card_name = ',SL[I]) > 0 then
                  PC[C].Name := CopyFromPos(SL[I],'alsa.card_name = "','"') Else
                if Pos('Profiles:', SL[I]) > 0 then
                  ProfileSub := True;
                If I < SL.Count -1 then
                  Inc(I) Else
                  break;
                if (IsADevice(SL[I]) = True) or (IsNewCard(SL[I]) = True) then
                  begin
                    Dec(I);
                    break;
                  end;
              end;
          end;
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
                        begin
                          ND.Typ := PDT_Monitor;
                          If ND.Device_Name = DefaultMic then
                            ND.DefaultSource := True;
                        end Else
                      if (Pos('alsa_input',SL[I]) > 0) then
                        begin
                          ND.Typ := PDT_Microfone;
                          ND.MicID := ND.ID;
                          If ND.Device_Name = DefaultMic then
                            ND.DefaultSource := True;
                        end  Else
                        begin
                          ND.Typ := PDT_Speaker;
                          ND.SpkID := ND.ID;   
                          If ND.Device_Name = DefaultSink then
                            ND.DefaultSink := True;
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
                    If Mode_Pipewire = True then
                      begin
                        If (ND.Typ = PDT_Recorder) and (Copy(ND.Media_Name,1,8)='loopback') then
                          begin
                            ND.Typ := PDT_LoopRec;
                            Delete(ND.Media_Name,1,8);
                          end Else
                        If (ND.Typ = PDT_Player) and (Copy(ND.Media_Name,1,8)='loopback') then
                          begin
                            ND.Typ := PDT_LoopSpk;
                            Delete(ND.Media_Name,1,8);
                          end;
                      end else
                      begin
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
                (Pos('Volume: ',SL[I]) > 0) or
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
                if (IsADevice(SL[I]) = True) or (IsNewCard(SL[I]) = True) then
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

procedure TDeviceMngt.FGetLinePoint(ADev: TDevice; var PlyP: TPoint; var RecP: TPoint; const RecUnderPl: Boolean = True);
begin
  PlyP.X := -1;
  PlyP.Y := -1;
  RecP.X := -1;
  RecP.Y := -1;
  //Eingangspfeil (offen) Links / Oben (Audioeingabe / Lautsprecher):
  if (PDT_Speaker and ADev.D.Typ <> 0) then
    begin
      PlyP.X := ADev.X + 10;
      PlyP.Y := ADev.Y + 6;
    end;    
  //Ausgangspfeil (offen) Rechts / Oben (Audioausgabe / Player):
  if (PDT_Player and ADev.D.Typ <> 0) then
    begin
      PlyP.X := ADev.X + ADev.Width;
      PlyP.Y := ADev.Y + 6;
    end;         
  //Ausgangspfeil (offen) Rechts / Oben (Audioausgabe / Player):
  if (PDT_Loop and ADev.D.Typ <> 0) then
    begin
      PlyP.X := ADev.X + ADev.Width div 2;
      If RecUnderPl = True then
        begin       
          PlyP.Y := ADev.Y;    
          RecP.Y := ADev.Y + ADev.Height;
        end Else
        begin     
          PlyP.Y := ADev.Y + ADev.Height;
          RecP.Y := ADev.Y;
        end;
      RecP.X := ADev.X + ADev.Width div 2;
    end;
  //Eingangspfeil (geschlossen) Rechts / Unten (Audioeingabe / Recorder):
  if (PDT_Recorder and ADev.D.Typ <> 0) then
    begin
      RecP.X := ADev.X + ADev.Width;
      RecP.Y := ADev.Y + ADev.Height -6;
    end;
  //Ausgangspfeil Unten (Audioausgabe / Mikrofon / Monitor):
  if (PDT_Microfone and ADev.D.Typ <> 0) or (PDT_Monitor and ADev.D.Typ <> 0) then
    begin
      RecP.X := ADev.X + 10;
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

procedure TDeviceMngt.FDrawOneConnection(Lower, Higher: TPoint; AColor: Integer; OneIsLoop: Boolean; ToLoop: Boolean);
begin
  FImage.Canvas.Pen.Color := AColor;
  FImage.Canvas.Pen.Width := 3;
  if not OneIsLoop then
    begin
      //Device -->
      FImage.Canvas.Line(Lower.X,Lower.Y,Lower.X+( (Higher.X - Lower.X) div 2)+20,Lower.Y);
      //Line
      FImage.Canvas.Line(Lower.X+( (Higher.X - Lower.X) div 2)+20,Lower.Y,Higher.X-30,Higher.Y);
      //--> Device
      FImage.Canvas.Line(Higher.X,Higher.Y,Higher.X-30,Higher.Y);
    end else
    begin      
      //Device -->
      If ToLoop then
        begin
          FImage.Canvas.Line(Higher.X-50,Lower.Y+5,Higher.X-45,Lower.Y);
          FImage.Canvas.Line(Higher.X-50,Lower.Y-5,Higher.X-45,Lower.Y);
        end else
        begin    
          FImage.Canvas.Line(Higher.X-45,Lower.Y+5,Higher.X-50,Lower.Y);
          FImage.Canvas.Line(Higher.X-45,Lower.Y-5,Higher.X-50,Lower.Y);
        end;
      FImage.Canvas.Line(Lower.X,Lower.Y,Higher.X,Lower.Y);
      //--> Device
      FImage.Canvas.Line(Higher.X,Lower.Y,Higher.X,Higher.Y);
    end;
end;

procedure TDeviceMngt.FDrawDeviceLines(var Con: TCable);
var
  P1, P2: TPoint;
  Loop, FromLoop: Boolean;
begin
  FromLoop := False;
  Loop := (Devs[Con.Input].D.Typ = PDT_Loop) or (Devs[Con.Output].D.Typ = PDT_Loop);
  FGetLinePoint(Devs[Con.Input],P1,P2, Loop);
  if Devs[Con.Input].D.Typ = PDT_Loop then
    begin
      if (Devs[Con.Output].D.Typ = PDT_Monitor) or (Devs[Con.Output].D.Typ = PDT_Microfone) then
        Con.Start := P2 Else
        Con.Start := P1;
    end Else
    begin
      Con.Start := P2;
    end;
  FGetLinePoint(Devs[Con.Output],P1,P2);
  if Devs[Con.Output].D.Typ = PDT_Loop then
    begin
      FromLoop := True;
      if (Devs[Con.Input].D.Typ = PDT_Monitor) or (Devs[Con.Input].D.Typ = PDT_Microfone) then
        Con.Goal := P2 Else
        Con.Goal := P1;
    end Else
      Con.Goal := P2;
  GetLowerHigher(Con.Start,Con.Goal,Con.Lower,Con.Higher);
  FDrawOneConnection(Con.Lower, Con.Higher,Con.Color, Loop, FromLoop);
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
  I, J, K : Integer;
  MaxLeftWidth, MaxRightWidth, LoopSpace: Integer;
  LeftH, RightH: Integer;
  Loops: Word;
  Loopw: Word;
  LoopTopH, LoopBtmH, LoopY: Word;
  LoopYs: Array of Word;
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
      Loops := 0;
      Loopw := 0;
      for I := 0 to Length(PD) -1 do
        if PDT_Loop and PD[I].Typ <> 0 then
          begin
            Inc(Loops);
            Loopw := Devs[I].Width;
          end;
      LoopSpace := Loopw * Loops;
      MainFRM.Constraints.MinWidth := MaxLeftWidth + MaxRightWidth + LoopSpace + 150;
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
              Devs[I].X := MainFrm.Width - MaxRightWidth - LoopSpace - 30;
              for J := 0 to Length(PD) -1 do
                if (PDT_Monitor and PD[J].Typ <> 0) and (PD[J].Device_Name = PD[I].Device_Name + '.monitor') then
                  begin
                    RightH := RightH -23;
                    Devs[J].Y := RightH;
                    RightH := RightH + (Devs[J].Height + 20); 
                    Devs[J].X := MainFrm.Width - MaxRightWidth - LoopSpace - 30;
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
              Devs[I].X := MainFrm.Width - MaxRightWidth - LoopSpace - 30;
              RightH := RightH + (Devs[I].Height + 20);
            end;
        end;
      If Loops > 0 then
        begin
          SetLength(LoopYs,Loops);
          Loops := 0;
          for I := 0 to Length(PD) -1 do
            begin
              if PDT_Loop and PD[I].Typ <> 0 then
                begin
                  LoopTopH := 0;    
                  LoopBtmH := 0;
                  LoopY := RightH div 2;
                  for J := 0 to High(PD) do
                    begin
                      If PD[I].PlayingOn = PD[J].ID then
                        LoopTopH := Devs[J].Y + (Devs[J].Height div 2);  
                      If PD[I].RecordingFrom = PD[J].ID then
                        LoopBtmH := Devs[J].Y + (Devs[J].Height div 2);
                      If (LoopBtmH > 0) and (LoopTopH > 0) then
                        begin
                          If LoopTopH < LoopBtmH then
                            begin
                              LoopYs[Loops] := (LoopBtmH - LoopTopH) div 2;
                              LoopY := LoopTopH + LoopYs[Loops];
                            end else
                            begin      
                              LoopYs[Loops] := (LoopTopH - LoopBtmH) div 2;
                              LoopY := LoopBtmH + LoopYs[Loops];
                            end;
                          break;
                        end;
                    end;
                  Devs[I].Y := LoopY;
                  Inc(Loops);
                end;
            end;
          //Sorge dafür, dass nie die gleiche Größe da ist:
          for Loops := High(LoopYs) downto 0 do
            if Loops > 0 then
              for LoopY := Loops -1 downto 0 do
                if LoopYs[Loops] = LoopYs[LoopY] then
                  LoopYs[Loops] := LoopYs[Loops] +1;   
          Loops := 0;
          LoopTopH := 0;
          for Loops := 0 to High(LoopYs) do
            begin        
              LoopBtmH := 65000;
              LoopY := 0;
              //Erhalte das kleinste Element:
              for I := 0 to High(LoopYs) do
                 If (LoopYs[I] < LoopBtmH) and (LoopYs[I] > LoopTopH) then
                   begin
                     LoopY := I;
                     LoopBtmH := LoopYs[I];
                   end;
              LoopTopH := LoopBtmH;
              J := -1;
              //Erhalte das passende Elemnt zur Indexnummer:
              while J < LoopY do
                begin
                  for K := 0 to High(PD) do
                    if PDT_Loop and PD[K].Typ <> 0 then
                      begin
                        Inc(J);
                        If J = LoopY then
                          Devs[K].X := MainFrm.Width - 15 - (Loopw * (Length(LoopYs) - Loops));
                      end;
                end;
            end;
          //Korrigiere, Loops, die zu klein sind, oder auf eiegen Positionen laufen:
          I := 0;
          for K := 0 to High(PD) do
            if (PDT_Loop and PD[K].Typ <> 0) and (Devs[K].X < MainFrm.Width - MaxRightWidth - LoopSpace - 30) then
              Inc(I);
          if I > 0 then   
            for K := 0 to High(PD) do
              if (PDT_Loop and PD[K].Typ <> 0) and (Devs[K].X < MainFrm.Width - MaxRightWidth - LoopSpace - 30) then
                begin
                  Devs[K].X := MainFrm.Width - 15 - LoopSpace - (Loopw * (I));
                  Dec(I);
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
        if (Devs[I].D.Vol_Muted) then
          Devs[I].Paint;
      for I := 0 to Length(Devs) -1 do
        if (not Devs[I].D.Vol_Muted) and (not Devs[I].Selected) then
          Devs[I].Paint;
      for I := 0 to Length(Devs) -1 do
        if (not Devs[I].D.Vol_Muted) and (Devs[I].Selected) then
          Devs[I].Paint;
      MainFRM.BI.Picture.Bitmap.Assign(FImage); //Oder: MainFRM.BI.Canvas.Draw(0,0,FImage);
      MainFrm.Update;
    end;
  FRepainting := False;
end;

procedure TDeviceMngt.LoadFromPulse(const ForceFullReload: Boolean = False);
var
  I, J, L: Integer;
  RedrawMe: Boolean;
  AM: TMenuItem;
begin
  RedrawMe := ReloadPulseDevices(PD, PC, FDefaultSink, FDefaultMic);
  If ForceFullReload = True then
    ReDrawMe := True;
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
        begin          { DEFAULT-MENU nicht mehr notwendig!
          if MainFrm.M3_DefSpeakers.Count > 0 then
            for I := MainFrm.M3_DefSpeakers.Count -1 downto 0 do
              MainFrm.M3_DefSpeakers.Delete(I);
          if MainFrm.M4_DefMicrofone.Count > 0 then
            for I := MainFrm.M4_DefMicrofone.Count -1 downto 0 do
              MainFrm.M4_DefMicrofone.Delete(I);        }
          if MainFrm.M5_CardConfig.Count > 0 then
            for I := MainFrm.M5_CardConfig.Count -1 downto 0 do
              begin
                if MainFrm.M5_CardConfig.Items[I].Count > 0 then
                  for J := MainFrm.M5_CardConfig.Items[I].Count -1 downto 0 do
                    MainFrm.M5_CardConfig.Items[I].Delete(J);
                MainFrm.M5_CardConfig.Delete(I);
              end;
          if Length(PC) > 0 then
            for I := 0 to Length(PC) -1 do
              if Length(PC[I].Props) > 0 then
                begin
                  AddMenuItem(MainFrm.M5_CardConfig,PC[I].Name,nil);
                  AM := MainFrm.M5_CardConfig.Items[I];
                  for J := 0 to Length(PC[I].Props) -1 do
                    begin
                      If ((MainFrm.M01_OP_HideCards.Checked = True) and (PC[I].Props[J].Avail = True)) or
                         (MainFrm.M01_OP_HideCards.Checked = False) then
                        begin
                          If (MainFrm.M01_OP_HideCards.Checked = False) and (PC[I].Props[J].Avail = False) then
                            AddMenuItem(AM,PC[I].Props[J].Name + ' (unused)', @MainFrm.M5_ItemClick) Else
                            AddMenuItem(AM,PC[I].Props[J].Name, @MainFrm.M5_ItemClick);
                          L := AM.Count -1;
                          AM.Items[L].Tag := I * 10000 + J;
                          If J = PC[I].CurrentProp then
                            AM.Items[L].Checked := True Else
                            AM.Items[L].Checked := False;
                        end;
                    end;
                end;
        end;
      for I := 0 to Length(PD) -1 do
        begin   { DEFAULT-MENU nicht mehr notwendig!
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
            end;      }
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
  I, J, K: Integer;
  AD: TPulseDevices;
  S, T: String;
  ADefaultSink, ADefaultMic: String;
begin
  if (AFileName <> '') and (FileExists(AFileName)) then
    begin
      Ini := TIniFile.Create(AFileName);
      try
        SetLength(AD,Ini.ReadInteger(Main,'Count',0));        
        ADefaultSink := Ini.ReadString(Main,'DefaultSink','');
        ADefaultMic := Ini.ReadString(Main,'DefaultMic','');
        if Length(AD) > 0 then
          begin
            for I := 0 to Length(AD) -1 do
              begin
                AD[I].Typ := Ini.ReadInteger(Main,IntToStr(I)+'_Typ',PDT_UnknownDev);
                AD[I].ID := Ini.ReadString(Main,IntToStr(I)+'_ID','');
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
            //Lade ausgewählte / gespeicherte Kartenauswahl:
            J := Ini.ReadInteger(Main,'CardsCount',0);
            If J > 0 then
              for I := 0 to J -1 do
                begin
                  S := Ini.ReadString(Main,IntToStr(I)+'_Card','');
                  If S <> '' then
                    If Mode_Pipewire = True then
                      RunCMD('pactl set-card-profile "'+S+'"') else
                      RunCMD('pacmd set-card-profile "'+S+'"');
                end;
            //Lade Virtuelle Sinks:
            for I := 0 to Length(AD) -1 do
              if (AD[I].Typ = PDT_Speaker) and (AD[I].VSink = True) then
                RunCmd('pactl load-module module-null-sink sink_name="'+AD[I].Device_Name+'" sink_properties=device.description="'+AD[I].Name+'"');  
            LoadFromPulse;       
            //Set Defaults:
            if ADefaultSink <> '' then
              if Mode_Pipewire = true then
                RunCMD('pactl set-default-sink "'+ADefaultSink+'"') else
                RunCMD('pacmd set-default-sink "'+ADefaultSink+'"');
            if FDefaultMic <> '' then
              if Mode_Pipewire = true then
                RunCMD('pactl set-default-source "'+ADefaultMic+'"') else
                RunCMD('pacmd set-default-source "'+ADefaultMic+'"');
            //Lade Loopbacks:
            for I := 0 to Length(AD) -1 do
              if AD[I].Typ = PDT_Loop then
                begin
                  //AD[I].ID := '';
                  for J := 0 to Length(AD) -1 do
                    if AD[I].RecordingFrom = AD[J].ID then
                      begin
                        for K := 0 to Length(PD) -1 do
                          If AD[J].Name = PD[K].Name then
                            begin
                              S := PD[J].ID;
                              break;
                            end;
                        break;
                      end;
                  for J := 0 to Length(AD) -1 do
                    if AD[I].PlayingOn = AD[J].ID then
                      begin
                        for K := 0 to Length(PD) -1 do
                          If AD[J].Name = PD[K].Name then
                            begin
                              T := PD[K].ID;
                              break;
                            end;
                        break;
                      end;
                  If (S <> '') and (T <> '') then
                    RunCMD('pactl load-module module-loopback source='+S+' sink='+T+' latency_msec='+IntToStr(LoopDelay)) Else
                    RunCMD('pactl load-module module-loopback latency_msec='+IntToStr(LoopDelay));
                end;
            //Set Volumes [Mic, Monitor, Rec, Player]:
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
            LoadFromPulse;
            //Add Cables from Player / Recorder:
            if Length(PD) > 0 then
              for I := 0 to Length(AD) -1 do
                if (AD[I].Typ = PDT_Player) then
                  begin
                    if Mode_Pipewire = true then
                      RunCmd('pactl move-sink-input '+AD[I].ID+' '+AD[I].PlayingOn) else
                      RunCmd('pacmd move-sink-input '+AD[I].ID+' '+AD[I].PlayingOn);
                  end Else
                if (AD[I].Typ = PDT_Recorder) then
                  begin     
                    if Mode_Pipewire = true then
                      RunCmd('pactl move-source-output '+AD[I].ID+' '+AD[I].RecordingFrom) else
                      RunCmd('pacmd move-source-output '+AD[I].ID+' '+AD[I].RecordingFrom);
                  end;
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
  pastr: String;
begin
  if Mode_Pipewire = True then
    begin
      pastr := 'pactl';
      if Mute then
        cmdstr := ' 1' Else
        cmdstr := ' 0';
    end else
    begin
      pastr := 'pacmd';
      if Mute then
        cmdstr := ' true' Else
        cmdstr := ' false';
    end;
  if (PD[AID].Typ = PDT_Player) then
    RunCmd(pastr+' set-sink-input-mute '+PD[AID].ID+cmdstr) Else
  if (PD[AID].Typ = PDT_Speaker) then
    RunCmd(pastr+' set-sink-mute '+PD[AID].ID+cmdstr) Else
  if (PD[AID].Typ = PDT_Microfone) or (Dev.PD[AID].Typ = PDT_Monitor) then
    RunCmd(pastr+' set-source-mute '+PD[AID].ID+cmdstr) Else
  if (PD[AID].Typ = PDT_Recorder) then
    RunCmd(pastr+' set-source-output-mute '+PD[AID].ID+cmdstr);
end;

procedure TDeviceMngt.SetVol(AID: Integer; AVol: Integer);
var
  IVol: Integer;  
  pastr: String;
begin        
  if Mode_Pipewire = True then
    pastr := 'pactl' else
    pastr := 'pacmd';
  IVol := -1;
  if (AVol <> -1) and (AVol >= 0) and (AVol <= MAX_VOL) then
    begin
      PD[AID].Volume := IntToStr(AVol);
      Devs[AID].D.Volume := IntToStr(AVol);
      IVol := Round((AVol / MAX_VOL) * 99957);
      if (IVol > -1) and (IVol < 99957) then
        if (PD[AID].Typ = PDT_Player) then
          RunCMD(pastr+' set-sink-input-volume '+PD[AID].ID+' '+IntToStr(IVol)) Else
        if (PD[AID].Typ = PDT_Speaker) then
          RunCMD(pastr+' set-sink-volume '+PD[AID].ID+' '+IntToStr(IVol)) Else
        if (PD[AID].Typ = PDT_Microfone) or (Dev.PD[AID].Typ = PDT_Monitor) then
          RunCMD(pastr+' set-source-volume '+PD[AID].Device_Name+' '+IntToStr(IVol)) Else
        if (PD[AID].Typ = PDT_Recorder) then
          RunCMD(pastr+' set-source-output-volume '+PD[AID].ID+' '+IntToStr(IVol));
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
        if Length(PD) > 0 then
          for I := 0 to Length(PD) -1 do
            begin
              If PD[I].DefaultSink = True then
                  Ini.WriteString(Main,'DefaultSink',PD[I].Device_Name);    
              If PD[I].DefaultSource = True then
                  Ini.WriteString(Main,'DefaultMic',PD[I].Device_Name);
              Ini.WriteInteger(Main,IntToStr(I)+'_Typ',PD[I].Typ);        
              Ini.WriteString(Main,IntToStr(I)+'_Ident',PD[I].Ident);
              Ini.WriteString(Main,IntToStr(I)+'_ID',PD[I].ID);
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
          if Length(PC) > 0 then
            begin
              Ini.WriteInteger(Main,'CardsCount',Length(PC));
              for I := 0 to Length(PC) -1 do
                Ini.WriteString(Main,IntToStr(I)+'_Card',PC[I].PulseName+'" "'+PC[I].Props[PC[I].CurrentProp].PulseName);
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
  S, pastr: String;
  Applied: Boolean;
begin
  if Mode_Pipewire = True then
    pastr := 'pactl' else
    pastr := 'pacmd';
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
                                RunCmd(pastr+' move-sink-input '+PD[FDeviceSelected].ID+' '+PD[I].Device_Name);
                                Applied := True;
                              end;
                PDT_Speaker: if (K = PDT_Player) or (K = PDT_LoopSpk) then
                              begin
                                //Speaker --> Anmwendung
                                RunCmd(pastr+' move-sink-input '+PD[I].ID+' '+PD[FDeviceSelected].Device_Name);
                                Applied := True;
                              end;
                PDT_Microfone, PDT_Monitor: if (K = PDT_Recorder) or (K = PDT_LoopRec) then
                              begin
                                //Mic OR Monitor --> Recorder or Loop (Rec)
                                If K = PDT_Recorder then
                                  S := PD[I].ID Else
                                  S := PD[I].MicID;
                                RunCmd(pastr+' move-source-output '+S+' '+PD[FDeviceSelected].Device_Name);
                                Applied := True;
                              end;
                PDT_Recorder, PDT_LoopRec: if (K = PDT_Microfone) or (K = PDT_Monitor) then
                              begin
                                //Recorder or Loop (Rec) --> MIC OR Monitor
                                If J = PDT_Recorder then
                                  S := PD[FDeviceSelected].ID Else
                                  S := PD[FDeviceSelected].MicID;
                                RunCmd(pastr+' move-source-output '+S+' '+PD[I].Device_Name);
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
  FDefaultSink := D.DefaultSink;
  FDefaultSource := D.DefaultSource;
  Selected := False;
  X := 20;
  Y := 50;
end;

procedure TDevice.Paint;
var
  Bitmap: TBitmap;
  TX: Integer;
  Col: Integer;
  S: String;
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
    TX := 5;
    if (PDT_Loop and D.Typ <> 0) then
      Bitmap.Canvas.Rectangle(1,1,FWidth-1,FHeight-1) else
    if (PDT_Speaker and D.Typ <> 0) or (PDT_Microfone and D.Typ <> 0) or (PDT_Monitor and D.Typ <> 0) then
      begin
        Bitmap.Canvas.Rectangle(10,1,FWidth-1,FHeight-1);
        TX := 15;
      end Else
      Bitmap.Canvas.Rectangle(1,1,FWidth-10,FHeight-1);
    //Eingangspfeil (offen) Links / Oben (Audioeingabe / Lautsprecher):
    if (PDT_Speaker and D.Typ <> 0) then
      begin
        Bitmap.Canvas.Line(1,1,10,6);
        Bitmap.Canvas.Line(1,11,10,6);
      end;          
    //Ausgangspfeil (offen) Rechts / Oben (Audioausgabe / Player):
    if (PDT_Player and D.Typ <> 0) then
      begin
        Bitmap.Canvas.Line(Fwidth -11,4,Fwidth -11,10);
        Bitmap.Canvas.Line(Fwidth -10,1,Fwidth,6);
        Bitmap.Canvas.Line(Fwidth -10,11,Fwidth,6);
      end;
    //Eingangspfeil (geschlossen) Rechts / Unten (Audioeingabe / Recorder):
    if (PDT_Recorder and D.Typ <> 0) then
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
        S := FMainCaption;
        If Length(S) > MAX_CAPSIZE+2 then
          begin
            S := Copy(S,1,MAX_CAPSIZE);
            S := S + '..';
          end;
        Bitmap.Canvas.TextOut(TX,3,S);
        S := FSecCaption;
        If Length(S) > MAX_CAPSIZE+2 then
          begin
            S := Copy(S,1,MAX_CAPSIZE);
            S := S + '..';
          end;
        Bitmap.Canvas.TextOut(TX,FTextHeight+3,S);
      end else
      begin
        S := FMainCaption;
        If Length(S) > MAX_CAPSIZE+2 then
          begin
            S := Copy(S,1,MAX_CAPSIZE);
            S := S + '..';
          end;
        Bitmap.Canvas.TextOut(TX,3,S);
      end;
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
  DSinkStr = '[DEF] ';
var
  S, SS, T, V: String;
begin
  //Setze die "Caption":
  If (FDefaultSink = True) or (FDefaultSource = True) then
    S := DSinkStr Else
    S := '';
  If (D.VSink = True) and (D.Typ <> PDT_Monitor) then
    S := S + VSinkStr;
  SS := S;
  case D.Typ of
    PDT_Speaker: S := S+'Speaker';
    PDT_Player: if (S <> '') and (S <> SS) then
                  S := S + ' / Player' Else
                  S := S + 'Player';
    PDT_Recorder: if (S <> '') and (S <> SS) then
                    S := S + ' / Rec' Else
                    S := S +'Rec';
    PDT_Microfone, PDT_Monitor: begin
                                  if PDT_Monitor and D.Typ <> 0 then
                                    T := 'Monitor' Else
                                    T := 'Mic';
                                  if (S <> '') and (S <> SS) then
                                    S := S + ' / '+T Else
                                    S := S + T;
                                end;
  end;
  FSecCaption := '';
  If D.Volume <> '' then
    V := ' ['+D.Volume+'%] ' Else
    V := '';
  if (D.Typ = PDT_Loop) then
    FMainCaption := 'L' Else
  if (D.Typ = PDT_Monitor) then
    FMainCaption := S+V Else
    begin
      FMainCaption := S+V+':';
      FSecCaption := D.Name;
    end;
end;

procedure TDevice.FSetDefaultSink(ADefault: Boolean);  
var
  pastr: String;
begin
  If (D.Typ <> PDT_Speaker) then
    exit;
  If (ADefault = True) and (FDefaultSink = False) then
    begin
      if Mode_Pipewire = True then
        pastr := 'pactl' else
        pastr := 'pacmd';
        RunCMD(pastr+' set-default-sink "'+D.Device_Name+'"');
        Dev.LoadFromPulse;
    end;  
  FDefaultSink := ADefault;
  D.DefaultSink := ADefault;
end;

procedure TDevice.FSetDefaultSource(ADefault: Boolean);
var
  pastr: String;
begin
  If (D.Typ <> PDT_Microfone) and (D.Typ <> PDT_Monitor) then
    exit;
  If (ADefault = True) and (FDefaultSource = False) then
    begin
      if Mode_Pipewire = True then
        pastr := 'pactl' else
        pastr := 'pacmd';
        RunCMD(pastr+' set-default-source "'+D.Device_Name+'"');
        Dev.LoadFromPulse;
    end;
  FDefaultSource := ADefault;
  D.DefaultSource := ADefault;
end;

function TDevice.GetWidth: Integer;
var
  L1, L2: Integer;
  S: String;
begin
  if FMainCaption = '' then
    FSetCaption;
  S := FMainCaption;
  If Length(S) > MAX_CAPSIZE+2 then
    begin
      S := Copy(S,1,MAX_CAPSIZE);
      S := S + '..';
    end;
  MainFrm.CalcLab.Caption := S;
  If (D.Typ = PDT_Loop) then
    begin
      Result := 15 + MainFrm.CalcLab.Canvas.TextWidth(S);
      FWidth := Result;
      exit;
    end;
  L1 := 25 + MainFrm.CalcLab.Canvas.TextWidth(S);
  S := FSecCaption;
  If Length(S) > MAX_CAPSIZE+2 then
    begin
      S := Copy(S,1,MAX_CAPSIZE);
      S := S + '..';
    end;
  MainFrm.CalcLab.Caption := S;
  L2 := 25 + MainFrm.CalcLab.Canvas.TextWidth(S);
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
  pastr: String;
begin
  if Mode_Pipewire = True then
    pastr := 'pactl' else
    pastr := 'pacmd';
  if Length(Dev.PD) > 0 then
    for I := 0 to Length(Dev.PD) -1 do
      if Dev.PD[I].Name = (Sender as TMenuItem).Caption then
        begin
          RunCMD(pastr+' set-default-sink "'+Dev.PD[(Sender as TMenuItem).Tag].Device_Name+'"');
          Dev.LoadFromPulse;
          break;
        end;
end;

procedure TMainFRM.M4_ItemClick(Sender: TObject);     
var
  I: Integer;     
  pastr: String;
begin
  if Mode_Pipewire = True then
    pastr := 'pactl' else
    pastr := 'pacmd';
  if Length(Dev.PD) > 0 then
    for I := 0 to Length(Dev.PD) -1 do
      if Dev.PD[I].Name = (Sender as TMenuItem).Caption then
        begin
          RunCMD(pastr+' set-default-source "'+Dev.PD[(Sender as TMenuItem).Tag].Device_Name+'"');
          Dev.LoadFromPulse;
          break;
        end;
end;

procedure TMainFRM.M5_ItemClick(Sender: TObject);
var
  I, J: Integer;
  pastr: String;
begin
  if Mode_Pipewire = True then
    pastr := 'pactl' else
    pastr := 'pacmd';
  I := (Sender as TMenuItem).Tag div 10000;
  J := (Sender as TMenuItem).Tag - (I * 10000);
  if (Length(Dev.PC) >= I) and (Length(Dev.PC[I].Props) >= J) then
    begin
       RunCMD(pastr+' set-card-profile "'+Dev.PC[I].PulseName+'" "'+Dev.PC[I].Props[J].PulseName+'"');
       Dev.LoadFromPulse;
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
    Ini.WriteBool(Ini_Main,'HideCards',M01_OP_HideCards.Checked);
    Ini.WriteBool(Ini_Main,'OnTop',M01_OP_OnTop.Checked);
    Ini.WriteBool(Ini_Main, 'Pipewire', Mode_Pipewire);     
    Ini.WriteInteger(Ini_Main,'LoopDelay',LoopDelay);
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
    M01_OP_AutoReload.Checked := Ini.ReadBool(Ini_Main,'AutoReload',True);
    M01_OP_HideCards.Checked := Ini.ReadBool(Ini_Main,'HideCards',True);
    AutoT_Cnt := 0;
    AutoT.Enabled := M01_OP_AutoReload.Checked;
    M6_Reload.Visible := not M01_OP_AutoReload.Checked;
    M01_OP_OnTop.Checked := Ini.ReadBool(Ini_Main,'OnTop',False);      
    If M01_OP_OnTop.Checked then
      FormStyle:=fsSystemStayOnTop Else
      FormStyle:=fsNormal;
    M01_OP_Mode_Pipewire.Checked := Ini.ReadBool(Ini_Main, 'Pipewire', False);
    Mode_Pipewire := M01_OP_Mode_Pipewire.Checked;
    LoopDelay := Ini.ReadInteger(Ini_Main, 'LoopDelay', 10);
    If LoopDelay < 0 then
      LoopDelay := 10;
    M01_OP_LoopDelay.Caption := 'Loop-Latency: '+IntToStr(LoopDelay)+'ms';
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
  If (ParamStr(1) <> '') and (FileExists(ParamStr(1))) then
    Dev.LoadFromFile(ParamStr(1)) Else
    Dev.LoadFromPulse;
  If ParamStr(2) = '-close' then
    Close;
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

procedure TMainFRM.M01_OP_AutoReloadClick(Sender: TObject);
begin
  M01_OP_AutoReload.Checked := not M01_OP_AutoReload.Checked;
  AutoT_Cnt := 0;
  AutoT.Enabled := M01_OP_AutoReload.Checked;
  M6_Reload.Visible := not M01_OP_AutoReload.Checked;
end;

procedure TMainFRM.M01_OP_HideCardsClick(Sender: TObject);
begin
  M01_OP_HideCards.Checked := not M01_OP_HideCards.Checked;
  Dev.LoadFromPulse(True);
end;

procedure TMainFRM.M01_OP_LoopDelayClick(Sender: TObject);
var
  Str: String;
begin
  Str:=InputBox('Loop Latency','Please enter a latency:','');
  If StrToIntDef(Str,-1) = -1 then
    begin
      ShowMessage('Please enter a valid number.');
      exit;
    end;
  LoopDelay := StrToInt(Str);
  M01_OP_LoopDelay.Caption := 'Loop-Latency: '+IntToStr(LoopDelay)+'ms';
end;

procedure TMainFRM.M01_OP_Mode_PipewireClick(Sender: TObject);
begin
  M01_OP_Mode_Pipewire.Checked := not M01_OP_Mode_Pipewire.Checked;
  Mode_Pipewire := M01_OP_Mode_Pipewire.Checked;
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
  RunCMD('pactl load-module module-loopback latency_msec='+IntToStr(LoopDelay));
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

procedure TMainFRM.M6_ReloadClick(Sender: TObject);
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

procedure TMainFRM.RC_SetDefClick(Sender: TObject);
begin
  If (Dev.PD[RC_Dev].Typ = PDT_Speaker) then
    Dev.Devs[RC_Dev].DefaultSink := True else
    Dev.Devs[RC_Dev].DefaultSource := True;
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
begin
  FCurMouseP.X := X;
  FCurMouseP.Y := Y;
  Dev.DoCableMove(FCurMouseP);
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
          if (IVol <> -1) and (IVol < MAX_VOL) then
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
              RC_Del.Visible := (Dev.PD[I].Typ = PDT_Loop) or (Dev.PD[I].VSink = True);
              RC_SetDef.Visible := ((Dev.PD[I].Typ = PDT_Speaker) or (Dev.PD[I].Typ = PDT_Microfone) or (Dev.PD[I].Typ = PDT_Monitor))
                                   and ((Dev.PD[I].DefaultSink = False) and (Dev.PD[I].DefaultSource = False));
              if (RC_Del.Visible = True) or
                 (RC_SetDef.Visible = True) then
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
