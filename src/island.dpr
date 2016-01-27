program island;

uses
  Vcl.Forms,
  Unit7 in 'Unit7.pas' {Form7},
  gfxrender in 'gfxrender.pas',
  Terrain in 'Terrain.pas',
  Unit1 in 'Unit1.pas' {Form1},
  Khrono in 'Khrono.pas',
  vectors in 'vectors.pas',
  Model3D in 'Model3D.pas',
  jsonhelper in 'jsonhelper.pas',
  Model3DBlend in 'Model3DBlend.pas',
  MilitiaAdventurer in 'MilitiaAdventurer.pas',
  Obstacles in 'Obstacles.pas',
  Trees in 'Trees.pas',
  TextureManager in 'TextureManager.pas',
  ObjLoader in 'ObjLoader.pas';

{$R *.res}

begin
  NeverSleepOnMMThreadContention := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm7, Form7);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
