unit MainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ExtCtrls, FMX.Controls.Presentation, System.Rtti, FMX.Grid.Style,
  FMX.Grid, FMX.ScrollBox, IdHTTP, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient;

type
  TFormMain = class(TForm)
    sgPosts: TStringGrid;
    scTitle: TStringColumn;
    scBody: TStringColumn;
    scUser: TStringColumn;
    scCount: TIntegerColumn;
    sgComments: TStringGrid;
    scName: TStringColumn;
    scCommentsBody: TStringColumn;
    scEmail: TStringColumn;
    scNr: TStringColumn;
    IdHTTP1: TIdHTTP;
    Panel1: TPanel;
    Label1: TLabel;
    Panel2: TPanel;
    Label2: TLabel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure sgPostsSelectCell(Sender: TObject; const ACol, ARow: Integer; var
        CanSelect: Boolean);
  private
    procedure updateComments(const aPostID: Integer);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses
  Entities, System.Generics.Collections, Aurelius.Json.DataSnap, Bcl.Json;

{$R *.fmx}

const
  postsURL = 'http://jsonplaceholder.typicode.com/posts';
  commentsURL = 'http://jsonplaceholder.typicode.com/comments?postId=';
  userURL = 'http://jsonplaceholder.typicode.com/users/';

procedure TFormMain.FormCreate(Sender: TObject);
var
  postsList: TObjectList<TPosts>;
  post: TPosts;
  response: string;
  user: TUsers;
  commentsList: TObjectList<TComments>;
begin
  sgPosts.RowCount:=0;
  sgComments.RowCount:=0;

  IdHTTP1:=TIdHTTP.Create(self);

  response:=IdHTTP1.Get(postsURL);

  postsList:=TJSON.Deserialize<TObjectList<TPosts>>(response);
  postsList.OwnsObjects:=True;

  for post in postsList do
  begin
    sgPosts.RowCount:=sgPosts.RowCount + 1;
    sgPosts.Cells[0, sgPosts.RowCount-1]:=post.id.ToString;
    sgPosts.Cells[1, sgPosts.RowCount-1]:=post.title;
    sgPosts.Cells[2, sgPosts.RowCount-1]:=post.body;

    response:=IdHTTP1.Get(commentsURL+post.id.ToString);
    commentsList:=TJSON.Deserialize<TObjectList<TComments>>(response);
    commentsList.OwnsObjects:=True;
    sgPosts.Cells[3, sgPosts.RowCount-1]:=commentsList.Count.ToString;
    commentsList.Free;

    response:=IdHTTP1.Get(userURL+post.userID.ToString);
    user:=TJSON.Deserialize<TUsers>(response);
    sgPosts.Cells[4, sgPosts.RowCount-1]:=user.name;
    user.address.geo.Free;
    user.address.Free;
    user.company.Free;
    user.Free;

  end;
  postsList.Free;

  if sgPosts.RowCount>0 then
  begin
    sgPosts.Selected:=0;
    updateComments(sgPosts.Cells[0,0].ToInteger);
  end
  else
    sgPosts.Selected:=-1;

end;

procedure TFormMain.sgPostsSelectCell(Sender: TObject; const ACol, ARow:
    Integer; var CanSelect: Boolean);
begin
  updateComments(sgPosts.Cells[0,ARow].ToInteger);
end;

procedure TFormMain.updateComments(const aPostID: Integer);
var
  commentsList: TObjectList<TComments>;
  comment: TComments;
  response: string;
begin
  sgComments.RowCount:=0;

  response:=IdHTTP1.Get(commentsURL+aPostID.ToString);
  commentsList:=TJson.Deserialize<TObjectList<TComments>>(response);
  commentsList.OwnsObjects:=True;
  for comment in commentsList do
  begin
    sgComments.RowCount:=sgComments.RowCount + 1;
    sgComments.Cells[0, sgComments.RowCount - 1]:=comment.name;
    sgComments.Cells[1, sgComments.RowCount - 1]:=comment.body;
    sgComments.Cells[2, sgComments.RowCount - 1]:=comment.email;
  end;

  commentsList.Free;

end;

end.
