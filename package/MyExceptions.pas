{------------------------------------------------------------------------------------------------------------------------
    This software is distributed under MPL 2.0 https://www.mozilla.org/en-US/MPL/2.0/ in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR ANY PARTICULAR PURPOSE.

    Copyright (C) Dmitry Morozov: dvmorozov@hotmail.com
                        LinkedIn: https://www.linkedin.com/in/dmitry-morozov-79490a59/
                        Facebook: https://www.facebook.com/dmitry.v.morozov
------------------------------------------------------------------------------------------------------------------------}
unit MyExceptions;

interface

uses
    Classes, SysUtils;

type
    { The caller asked for something this code does not support - a value a user
      typed, a file that is not there. An ordinary outcome, and it deserves a
      message aimed at whoever is reading. }
    EUserException = class(Exception);

    { An invariant this code believes about ITSELF does not hold. Being wrong
      here means a defect, not a request that cannot be served, and the two are
      kept distinct so a catch-all written for the first cannot silently absorb
      the second. }
    EInternalCheckFailed = class(Exception);

{ WHY THESE AND NOT `Assert`.

  `Assert` is compiled out of a release build. So the checks relied on to catch a
  broken invariant are exactly the checks ABSENT from the build users run: a
  violated invariant that would stop the program on a developer's machine instead
  runs on, producing a plausible wrong number, in the one situation where
  diagnosing it is hardest. For a fitting library that is the worst possible
  failure, because a plausible wrong number is indistinguishable from a right one
  and gets used. A check that only runs when it is not needed is not a check.

  These are unconditional. Nothing here depends on a build flag.

  WHY THEY LIVE IN THIS UNIT rather than one of their own. This package has its
  own release cycle and its own licence and must build with nothing beside it;
  reaching into the application that consumes it for a checks unit would invert
  that. `MyExceptions` is already linked into everything here and already owns
  the question "which kind of error is this", so the answer belongs with it.

  WHAT THEY DO NOT DO, and the difference is deliberate: they raise but do not
  log. The consuming application's `Common/checks.pas` logs at the point of
  failure first, because an exception can be reworded or swallowed several layers
  up while a log entry cannot. This package has no logger and must not acquire
  one, so the raise is all there is - and the description below is therefore the
  whole record of what went wrong.

  WHAT THEY ARE NOT FOR. Not invalid input, and not conditions this code is
  expected to meet - those are ordinary outcomes and get `EUserException`. These
  are for statements the code believes must be true about itself.

  THE DESCRIPTION SAYS WHAT WAS EXPECTED, in terms of the domain rather than of
  the expression: 'an amplitude is never negative' is useful in a report, 'A >= 0'
  is what the next line of code already says. }

{ Fails unless ACondition holds. }
procedure CheckThat(ACondition: boolean; const ADescription: string);

{ Fails unless AObject is assigned. AName is what the thing IS, so the message
  reads "the user interaction is missing" rather than naming a field. }
procedure CheckAssigned(AObject: TObject; const AName: string);

{ Fails unless AIndex is a valid position in a collection of ACount items.
  Reports the offending index and the size, because an off-by-one and a wildly
  wrong index are different defects and the numbers distinguish them. }
procedure CheckIndex(AIndex, ACount: longint; const AWhat: string);

implementation

{ One place that raises, so the three cannot drift apart. }
procedure Fail(const AMessage: string);
begin
    raise EInternalCheckFailed.Create(AMessage);
end;

procedure CheckThat(ACondition: boolean; const ADescription: string);
begin
    if not ACondition then
        Fail(ADescription);
end;

procedure CheckAssigned(AObject: TObject; const AName: string);
begin
    if not Assigned(AObject) then
        Fail(AName + ' is missing when it is required');
end;

procedure CheckIndex(AIndex, ACount: longint; const AWhat: string);
begin
    if (AIndex < 0) or (AIndex >= ACount) then
        Fail(Format('%s: index %d is outside 0..%d', [AWhat, AIndex, ACount - 1]));
end;

initialization
end.
