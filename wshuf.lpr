(**
        shuffles a list of strings based on weights using
        this weighted shuffle algorithm:
        https://softwareengineering.stackexchange.com/questions/233541/how-to-implement-a-weighted-shuffle
*)
program wshuf;

{$mode objfpc}{$H+}

uses {$IFDEF UNIX} {$IFDEF UseCThreads}
  cthreads, {$ENDIF} {$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  Math,
  UnixRandom,
  WeightedRandom;

type

  { TWShuf }

  TWShuf = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

  { TWShuf }

  procedure TWShuf.DoRun;
  var
    ErrorMsg: string;
    inputLines: TStringList;
    outputLines: TStringList;
    i: integer;
    randomIndex: integer;
    inputFile: string;
    numLines: integer = 0;
    separator: Char = ';';
    verbose: boolean = False;

  begin
    // check for file argument
    if ParamCount = 0 then
    begin
      writeln('file name must be specified');
      Terminate(1);
      Exit;
    end;

    // quick check parameters
    ErrorMsg := CheckOptions('hnsv:', 'help lines separator verbose:');
    if ErrorMsg <> '' then
    begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse arguments
    inputFile := ParamStr(ParamCount);

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if HasOption('n', 'lines') then
    begin
      numLines := StrToInt(GetOptionValue('n', 'lines'));
    end;

    if HasOption('s', 'separator') then
    begin
      separator := GetOptionValue('s', 'separator')[1];
    end;

    if HasOption('v', 'verbose') then
    begin
      verbose := True;
    end;

    randomize();

    // load the file into the stringlist
    inputLines := TStringList.Create;
    inputLines.NameValueSeparator := separator;

    try
      inputLines.LoadFromFile(inputFile);
    except
      on E: EInOutError do
        writeln('File handling error occured. Reason: ', E.Message);
    end;

    outputLines := TStringList.Create;
    outputLines.NameValueSeparator := separator;

    // should we limit the output to numLines or use the whole input file?
    if (numLines = 0) or (numLines > inputLines.Count) then
    begin
      numLines := inputLines.Count;
    end
    else
    begin
      numLines := numLines;
    end;

    for i := 0 to numLines - 1 do
    begin
      randomIndex := getWeightedRandomIndex(inputLines);
      writeln(inputLines.ValueFromIndex[randomIndex]);
      if ('0' <> inputLines.ValueFromIndex[randomIndex]) then
      begin
         outputLines.Add(inputLines[randomIndex]);
         inputLines[randomIndex] := IntToStr(0);
      end;
    end;

    for i := 0 to outputLines.Count - 1 do
    begin
      if (False = verbose) then
      begin
           writeln(outputLines.Names[i]);
      end
      else
      begin
          writeln(outputLines[i]);
      end;
    end;

    inputLines.Free;
    outputLines.Free;

    // stop program loop
    Terminate;
  end;


  constructor TWShuf.Create(TheOwner: TComponent);
  begin
    inherited Create(TheOwner);
    StopOnException := True;
  end;

  destructor TWShuf.Destroy;
  begin
    inherited Destroy;
  end;

  procedure TWShuf.WriteHelp;
  begin
    { add your help code here }
    writeln('Usage: ', ExeName, ' -h');
  end;

var
  Application: TWShuf;

{$R *.res}

begin
  Application := TWShuf.Create(nil);
  Application.Title := 'wshuf';
  Application.Run;
  Application.Free;
end.
