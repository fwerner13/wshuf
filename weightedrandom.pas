unit WeightedRandom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

function getWeightedRandomIndex(items: TStringList): integer;

implementation

function getWeightedRandomIndex(items: TStringList): integer;
var
  tmpRand: integer;
  totalValues: integer = 0;
  i: integer;
begin
  // calculate total weight values
  for i := 0 to items.Count - 1 do
  begin
    totalValues := totalValues + StrToInt(items.ValueFromIndex[i]);
  end;

  tmpRand := random(totalValues);

  for i := 0 to items.Count - 1 do
  begin

    tmpRand := tmpRand - StrToInt(items.ValueFromIndex[i]);
    if tmpRand < 0 then
    begin
      Result := i;
      Break;
    end;
  end;
end;

end.
