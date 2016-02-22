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
  MilitiaAdventurer in 'MilitiaAdventurer.pas',
  Obstacles in 'Obstacles.pas',
  TextureManager in 'TextureManager.pas',
  ObjLoader in 'ObjLoader.pas',
  Shadows in 'Shadows.pas',
  shaders in 'shaders.pas',
  Trees in 'Trees.pas';

{$R *.res}

begin
  NeverSleepOnMMThreadContention := true;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm7, Form7);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
