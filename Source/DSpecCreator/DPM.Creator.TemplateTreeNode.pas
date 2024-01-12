unit DPM.Creator.TemplateTreeNode;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.ComCtrls,
  DPM.Core.Spec.Interfaces;

type
   TNodeType = (ntTemplateHeading, ntBuildHeading, ntDesignHeading, ntRuntimeHeading,
               ntSourceHeading, ntFileHeading, ntLibHeading, ntSeachPathHeading,
               ntDependencyHeading,
               ntBuild, ntDesign, ntRuntime,
               ntSource, ntFile, ntLib, ntSeachPath,
               ntDependency );


  TTemplateTreeNode = class (TTreeNode)
  public
    NodeType : TNodeType;
    OnNewText : string;
    OnNewClick : TNotifyEvent;
    OnDeleteText : string;
    OnDeleteClick : TNotifyEvent;

    TemplateHeading: Boolean;
    Template: ISpecTemplate;
    build: ISpecBuildEntry;
    bplEntry: ISpecBPLEntry;
    fileEntry: ISpecFileEntry;
    searchpath: ISpecSearchPath;
    dependency: ISpecDependency;

    function CategoryNode: TTemplateTreeNode;
    function IsHeading: Boolean;
    function IsBuild: Boolean;
    function IsBuildHeading: Boolean;
    function IsDesign: Boolean;
    function IsDesignHeading: Boolean;
    function IsRuntime: Boolean;
    function IsRuntimeHeading: Boolean;
    function IsSource: Boolean;
    function IsSourceHeading: Boolean;
    function IsFileEntry: Boolean;
    function IsFileEntryHeading: Boolean;
    function IsLibEntry: Boolean;
    function IsLibEntryHeading: Boolean;
    function IsSearchPath: Boolean;
    function IsSearchPathHeading: Boolean;
    function IsDependency: Boolean;
    function IsDependencyHeading: Boolean;

    procedure DeleteBuild;
    procedure DeleteSource;
    procedure DeleteFileEntry;
    procedure DeleteLibEntry;
    procedure DeleteDesign;
    procedure DeleteRuntime;
    procedure DeleteSearchPath;
    procedure DeleteDependency;

  end;

implementation

{ TTemplateTreeNode }

function TTemplateTreeNode.CategoryNode: TTemplateTreeNode;
begin
  if IsHeading then
    Result := Self
  else if (Parent as TTemplateTreeNode).IsHeading then
    Result := (Parent as TTemplateTreeNode)
  else if (Parent as TTemplateTreeNode).IsHeading then
    Result := (Parent.Parent as TTemplateTreeNode)
  else
    raise Exception.Create('Category node not found');
end;

procedure TTemplateTreeNode.DeleteBuild;
begin
  Template.DeleteBuildEntryById(Build.Id);
end;

procedure TTemplateTreeNode.DeleteDependency;
begin
  Template.DeleteDependencyById(dependency.Id);
end;

procedure TTemplateTreeNode.DeleteDesign;
begin
  if not IsDesign  then
    raise Exception.Create('Node is not of type Design');

  Template.DeleteDesignBplBySrc(bplEntry.Source);
end;

procedure TTemplateTreeNode.DeleteFileEntry;
begin
  if not IsFileEntry  then
    raise Exception.Create('Node is not of type File');

  Template.DeleteFiles(fileEntry.Source);
end;

procedure TTemplateTreeNode.DeleteLibEntry;
begin
  if not IsLibEntry  then
    raise Exception.Create('Node is not of type Lib');
  Template.DeleteLib(fileEntry.Source);
end;

procedure TTemplateTreeNode.DeleteRuntime;
begin
  if not IsRuntime  then
    raise Exception.Create('Node is not of type Runtime');
  Template.DeleteRuntimeBplBySrc(bplEntry.Source);
end;

procedure TTemplateTreeNode.DeleteSearchPath;
begin
  Template.DeleteSearchPath(searchpath.Path);
end;

procedure TTemplateTreeNode.DeleteSource;
begin
  if not IsSource  then
    raise Exception.Create('Node is not of type Source');
  Template.DeleteSource(fileEntry.Source);
end;

function TTemplateTreeNode.IsBuild: Boolean;
begin
  Result := NodeType = ntBuild;
end;

function TTemplateTreeNode.IsBuildHeading: Boolean;
begin
  Result := NodeType = ntBuildHeading;
end;

function TTemplateTreeNode.IsDependency: Boolean;
begin
  Result := NodeType = ntDependency;
end;

function TTemplateTreeNode.IsDependencyHeading: Boolean;
begin
  Result := NodeType = ntDependencyHeading;
end;

function TTemplateTreeNode.IsDesign: Boolean;
begin
  Result := NodeType = ntDesign;
end;

function TTemplateTreeNode.IsDesignHeading: Boolean;
begin
  Result := NodeType = ntDesignHeading;
end;

function TTemplateTreeNode.IsFileEntry: Boolean;
begin
  Result := NodeType = ntFile;
end;

function TTemplateTreeNode.IsFileEntryHeading: Boolean;
begin
  Result := NodeType = ntFileHeading;
end;

function TTemplateTreeNode.IsHeading: Boolean;
begin
  Result := NodeType in [ntTemplateHeading, ntBuildHeading, ntDesignHeading, ntRuntimeHeading,
               ntSourceHeading, ntFileHeading, ntLibHeading, ntSeachPathHeading,
               ntDependencyHeading];
end;

function TTemplateTreeNode.IsLibEntry: Boolean;
begin
  Result := NodeType = ntLib;
end;

function TTemplateTreeNode.IsLibEntryHeading: Boolean;
begin
  Result := NodeType = ntLibHeading;
end;

function TTemplateTreeNode.IsRuntime: Boolean;
begin
  Result := NodeType = ntRuntime;
end;

function TTemplateTreeNode.IsRuntimeHeading: Boolean;
begin
  Result := NodeType = ntRuntimeHeading;
end;

function TTemplateTreeNode.IsSearchPath: Boolean;
begin
  Result := NodeType = ntSeachPath;
end;

function TTemplateTreeNode.IsSearchPathHeading: Boolean;
begin
  Result := NodeType = ntSeachPathHeading;
end;

function TTemplateTreeNode.IsSource: Boolean;
begin
  Result := NodeType = ntSource;
end;

function TTemplateTreeNode.IsSourceHeading: Boolean;
begin
  Result := NodeType = ntSourceHeading;
end;

end.
