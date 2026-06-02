unit DPM.Creator.TemplateTreeNode;

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.ComCtrls,
  Vcl.ActnList,
  DPM.Core.Spec.Interfaces;

type
   TNodeType = (ntTemplateHeading,
               ntSourceHeading, ntBuildHeading, ntDesignHeading, ntDependencyHeading,
               ntSource, ntBuild, ntDesign, ntDependency );


  TTemplateTreeNode = class (TTreeNode)
  public
    NodeType : TNodeType;
    AddAction : TAction;
    DeleteAction : TAction;

    TemplateHeading: Boolean;
    Template: ISpecTemplate;
    build: ISpecBuildEntry;
    designEntry: ISpecDesignEntry;
    sourceEntry: ISpecSourceEntry;
    dependency: ISpecDependency;

    function CategoryNode: TTemplateTreeNode;
    function IsHeading: Boolean;
    function IsBuild: Boolean;
    function IsBuildHeading: Boolean;
    function IsDesign: Boolean;
    function IsDesignHeading: Boolean;
    function IsSource: Boolean;
    function IsSourceHeading: Boolean;
    function IsDependency: Boolean;
    function IsDependencyHeading: Boolean;

    procedure DeleteBuild;
    procedure DeleteSource;
    procedure DeleteDesign;
    procedure DeleteDependency;

    procedure DeleteEntry;

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
  Template.DeleteBuildEntry(build.Project);
end;

procedure TTemplateTreeNode.DeleteDependency;
begin
  Template.DeleteDependency(dependency.Id);
end;

procedure TTemplateTreeNode.DeleteDesign;
begin
  if not IsDesign  then
    raise Exception.Create('Node is not of type Design');

  Template.DeleteDesignEntry(designEntry.Project);
end;

procedure TTemplateTreeNode.DeleteEntry;
begin
  case NodeType of
    ntBuild: DeleteBuild;
    ntDesign: DeleteDesign;
    ntSource: DeleteSource;
    ntDependency: DeleteDependency;
  else
    raise Exception.Create('DeleteEntry called on non entry node');
  end;
end;

procedure TTemplateTreeNode.DeleteSource;
begin
  if not IsSource  then
    raise Exception.Create('Node is not of type Source');
  Template.DeleteSource(sourceEntry.Source);
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

function TTemplateTreeNode.IsHeading: Boolean;
begin
  Result := NodeType in [ntTemplateHeading, ntSourceHeading, ntBuildHeading,
               ntDesignHeading, ntDependencyHeading];
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
