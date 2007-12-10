object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Teste'
  ClientHeight = 654
  ClientWidth = 1058
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  WindowState = wsMaximized
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object imgMain: TImage
    Left = 16
    Top = 16
    Width = 800
    Height = 600
    Stretch = True
  end
  object imgBlack: TImage
    Left = 822
    Top = 8
    Width = 200
    Height = 150
    Stretch = True
  end
  object imgBlue: TImage
    Left = 822
    Top = 476
    Width = 200
    Height = 150
    Stretch = True
  end
  object imgRed: TImage
    Left = 822
    Top = 164
    Width = 200
    Height = 150
    Stretch = True
  end
  object imgGreen: TImage
    Left = 822
    Top = 320
    Width = 200
    Height = 150
    Stretch = True
  end
  object btnLoadImage: TButton
    Left = 27
    Top = 621
    Width = 113
    Height = 25
    Action = actLoadImage
    TabOrder = 1
  end
  object btnReloadImage: TButton
    Left = 156
    Top = 621
    Width = 113
    Height = 25
    Action = actReloadImage
    TabOrder = 2
  end
  object btnAction: TButton
    Left = 672
    Top = 622
    Width = 126
    Height = 25
    Action = actSeparateColors
    Default = True
    TabOrder = 0
  end
  object aclMain: TActionList
    Left = 16
    Top = 16
    object actLoadImage: TAction
      Caption = 'Carregar Imagem'
      OnExecute = actLoadImageExecute
    end
    object actReloadImage: TAction
      Caption = 'Recarregar Imagem'
      OnExecute = actReloadImageExecute
    end
    object actSeparateColors: TAction
      Caption = 'Separar Cores'
      OnExecute = actSeparateColorsExecute
    end
    object actThinStep: TAction
      Caption = 'Afinamento (Passo)'
      OnExecute = actThinStepExecute
    end
    object actStaircaseRemoval: TAction
      Caption = 'Remo'#231#227'o de Escadas'
      OnExecute = actStaircaseRemovalExecute
    end
    object actFindXAxis: TAction
      Caption = 'Eixo X'
      OnExecute = actFindXAxisExecute
    end
    object actFindYAxis: TAction
      Caption = 'Eixo Y'
      OnExecute = actFindYAxisExecute
    end
  end
  object dlgOpen: TOpenDialog
    DefaultExt = '*.bmp'
    Filter = 'Imagem Bitmap|*.bmp'
    Left = 48
    Top = 16
  end
end
