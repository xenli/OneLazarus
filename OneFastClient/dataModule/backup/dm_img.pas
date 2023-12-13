unit dm_img;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type

  { TdmImg }

  TdmImg = class(TDataModule)
    img16: TImageList;
    img32: TImageList;
    img64: TImageList;
  private

  public

  end;

var
  dmImg: TdmImg;

implementation

{$R *.lfm}

end.
