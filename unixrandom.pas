{$ifdef UNIX}
(**
	initializes PRNG with data read from /dev/random

	Randomize initializes the pseudo-random number generator
	by storing a value read from /dev/random to system.randSeed.
	If reading fails, system.randomize will be used instead.

        source: https://wiki.freepascal.org/Dev_random
*)
unit UnixRandom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure randomize();

implementation
  procedure randomize();
  const
    // file name for random(4) device
    // used /dev/urandom because /dev/random would slow down extremely if
    // the program is called multiple times
    randomDeviceName = '/dev/urandom';
  var
    /// reading buffer
    // same type as system.randSeed
    randomNumber: cardinal;
    // file handle
    randomReader: file of cardinal;
  begin
    assign(randomReader, randomDeviceName);
    {$push}
    // turn off run-time error generation
    {$IOChecks off}
    reset(randomReader);

    // will possibly cause the error
    // EInOutError: Read past end of file
    // if /dev/random is depleted
    if IOResult() = 0 then
    begin
      read(randomReader, randomNumber);

      if IOResult = 0 then
      begin
        system.RandSeed:=randomNumber;
      end
      else
      begin
	// do not call one-self => fully qualified identfier
        system.randomize;
      end;
      close(randomReader);
    end
    {$pop}
    else
    begin
      // do not call one-self => fully qualified identfier
      system.randomize;
    end;

  end;

end.
{$else}
{$hint program does not use randomize based on /dev/random}
{$endif}
