object SQLiteConnection: TSQLiteConnection
  OldCreateOrder = True
  Height = 198
  Width = 282
  object AureliusConnection1: TAureliusConnection
    DriverName = 'SQLite'
    Params.Strings = (
      'Database=.\database.db')
    Left = 64
    Top = 64
  end
end
