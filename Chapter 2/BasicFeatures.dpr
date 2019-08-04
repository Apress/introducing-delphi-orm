program BasicFeatures;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Aurelius.Engine.ObjectManager,
  Aurelius.Drivers.Interfaces,
  System.Generics.Collections,
  Aurelius.Mapping.Attributes,
  Aurelius.Types.Blob,
  Aurelius.Global.Config,
  Aurelius.Engine.DatabaseManager,
  Aurelius.Criteria.Base,
  Aurelius.Criteria.Linq,
  Aurelius.Criteria.Projections,
  System.DateUtils,
  System.Variants,
  ConnectionModule in 'ConnectionModule.pas' {SQLiteConnection: TDataModule},
  EntitiesLazy in 'EntitiesLazy.pas';

var
  objManager: TObjectManager;
  dbConnection: IDBConnection;
  dbManager: TDatabaseManager;
  post1,
  post,
  post2: TPost;
  config: TGlobalConfigs;
  user: TUser;
  retrPost: TPost;
  postList: TList<TPost>;
  id: TGUID;
  retrUser: TUser;
  postsList: TObjectList<TPost>;
  projResults: TObjectList<TCriteriaResult>;
  projRes: TCriteriaResult;
begin
  ReportMemoryLeaksOnShutdown:=True;
  try
    Writeln('Starting...');
    writeln;

    config:=TGlobalConfigs.GetInstance;
    //config.AutoMappingMode:=TAutomappingMode.Full;

    SQLiteConnection:=TSQLiteConnection.Create(nil);
    dbConnection:=SQLiteConnection.CreateConnection;
    dbManager:=TDatabaseManager.Create(dbConnection);
    dbManager.BuildDatabase;
    dbManager.Free;

    objManager:=TObjectManager.Create(dbConnection);
    // Add User
    user:=TUser.Create;
    user.Name:='User '+random(100).toString;
    objManager.Save(user);
    id:=user.ID;
    Writeln('User '+GUIDToString(user.ID)+' added');
    writeln;

    // Add Post 1
    post1:=TPost.Create;
    post1.User:=user;
    post1.Content:='Post 1';
    post1.DateTime:= Now;
    objManager.Save(post1);
    objManager.Flush;

    Writeln('Post 1 added');
    writeln;

    // Add Post 2
    post2:=TPost.Create;
    post2.User:=user;
    post2.Content:='Post 2';
    post2.DateTime:= Now;
//    post2.DateTime:=EncodeDate(2019, 1, 30); // To test Group Projection
    objManager.Save(post2);
    objManager.Flush;
    Writeln('Post 2 added');
    writeln;

    // Retrieve post 1
    post:=objManager.Find<TPost>(post1.ID);
    Writeln('User associated to Post 1: '+GUIDToString(post.User.ID));

    // Retrieve user so in eager loading, the posts are retrieved
    retrUser:=objManager.Find<TUser>(id);
    postList:=retrUser.Posts;

    // Retrieve Posts for user
    Writeln('Retrieving posts ('+postList.Count.ToString+' found)...');
    for retrPost in retrUser.Posts do
      Writeln(retrPost.Content.Value);
    Writeln('Posts retrieved');
    writeln;

    // Simple Query
    Writeln('Retrieving simple query...');
    postsList:=objManager.Find<TPost>
                         .List;

    for retrPost in postsList do
      Writeln(retrPost.Content.Value);

    postsList.Free;

    // Query for DateTime
    Writeln('Retrieving DateTime query...');
    postsList:=objManager.Find<TPost>
                         .Add(Linq['DateTime'] = Now)
                         .List;

    for retrPost in postsList do
      Writeln(retrPost.Content.Value);

    postsList.Free;

    // Query for Posts and User
    Writeln('Retrieving Posts-User query...');
    postsList:=objManager.Find<TPost>
                         .Add(Linq['DateTime'] = Now)
                         .CreateAlias('User', 'user')
                           .Add(Linq['user.Name'] = 'John')
                         .List;

    for retrPost in postsList do
      Writeln(retrPost.Content.Value);

    postsList.Free;

    // Projection for Count
    Writeln('Retrieving Count projection (TProjections)...');
    projResults:=objManager.Find<TPost>
                           .Select(TProjections.ProjectionList
                                     .Add(TProjections.Count('ID'))
                                  )
                           .ListValues;
    for projRes in projResults do
      if projRes.Values[0] <> Null then
        Writeln(projRes.Values[0]);
    projResults.Free;

    // Projection for Group
    Writeln('Retrieving Group projection...');
    projResults:=objManager.Find<TPost>
                           .Select(TProjections.ProjectionList
                                     .Add(TProjections.Count('ID').As_('Num'))
                                     .Add(TProjections.Month('DateTime').As_('Month'))
                                     .Add(TProjections.Group(
                                       TProjections.Month('DateTime')
                                     ))
                                  )
//                           .CreateAlias('User','u')
//                             .Add(Linq['u.Name'] = 'John')
                           .ListValues;
    for projRes in projResults do
    begin
      if (projRes.Values[0] <> Null) and (projRes.Values[1] <> Null) then
        Writeln('Num: '+VarToStr(projRes.Values[0 {'Num'}]) +
                 ' | Month: '+VarToStr(projRes.Values[1{'Month'}]));
    end;

    projResults.Free;

    //

    objManager.Free;

    SQLiteConnection.Free;
    Writeln('Finished....Press Enter');
    readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
