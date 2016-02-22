unit DebugHelper;

interface uses
  Windows, Classes;

implementation

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

initialization
  TThread.CreateAnonymousThread(DebugProc).Start;

end.
