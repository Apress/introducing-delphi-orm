object ServerContainer: TServerContainer
  OldCreateOrder = False
  Height = 210
  Width = 431
  object SparkleHttpSysDispatcher: TSparkleHttpSysDispatcher
    Active = True
    Left = 72
    Top = 16
  end
  object XDataServer: TXDataServer
    BaseUrl = 'http://+:2001/callcentre'
    Dispatcher = SparkleHttpSysDispatcher
    Pool = XDataConnectionPool
    DefaultEntitySetPermissions = [List, Get]
    EntitySetPermissions = <>
    Left = 216
    Top = 16
  end
  object XDataConnectionPool: TXDataConnectionPool
    Connection = AureliusConnection
    Left = 216
    Top = 72
  end
  object AureliusConnection: TAureliusConnection
    DriverName = 'SQLite'
    Params.Strings = (
      'Database=..\..\database.db')
    Left = 216
    Top = 128
  end
end
