unit DebugHelper;

interface uses
  Windows, Classes;

implementation

function IsDebuggerPresent: boolean; stdcall; external 'kernel32.dll';

procedure DebugProc;
begin
  if not IsDebuggerPresent then
    Exit;
  repeat
    if GetKeyState(VK_F12) < 0 then
      DebugBreak;
    Sleep(1);
  until false;
end;

var
  tid: Cardinal;

initialization
  BeginThread(nil, 0, @DebugProc, nil, 0, tid);

end.
