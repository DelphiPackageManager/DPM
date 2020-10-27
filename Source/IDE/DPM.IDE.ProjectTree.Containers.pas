unit DPM.IDE.ProjectTree.Containers;

interface

uses
  System.Classes,
  System.Rtti;


type
  //we don't actually need to implement these, just need them to get the TContainer right
  //NOTE : The guids were discovered via RTTI - do not change unless they change in a later IDE version
  IModelContainer = interface
    ['{DAA1E441-B99A-11D1-931D-00C04FB17A5E}']
  end;

  IGraphLocation = interface
    ['{2EEFC3B0-01CF-11D1-9ADA-0000C0091B45}']
  end;

  IGraphData = interface
    ['{F0DDFBA3-8EA2-491E-B63D-C6023955E3F6}']
  end;

  ICustomProjectGroupProject = interface
    ['{F567E16A-DA4E-459F-9718-9A51559FFA0B}']
  end;

  //Since we don't have dcu's or dcp for the container classes, we are doing a direct
  //cast to our container class below, and using RTTI to access the private fields.
  //hacky but works!

  {
    The class heirachy looks like this

   TInterfacedObject
    TBaseContainer
      TStdContainer
        TStdFileContainer
          TStdProjectContainer
            TStdDelphiProjectContainer
      TStdContainerCategory

      The idea here is to match the public interface of container classes which we got via RTTI.
      This is actually TStdProjectContainer but we can use it for other container types
      provided we don't use props that don't exist.

      Seems to work fine in XE7 at least!
  }

  TProjectTreeContainer = class(TInterfacedObject)
  private
    class var
      FRttiContext : TRttiContext;
      FCategoryRttiType : TRttiInstanceType;
      FContainerConstructor : TRttiMethod;
  private
    function GetImageIndex : Integer;
    procedure SetImageIndex(const Value : Integer);
    function GetDisplayName : string;

    class constructor Create;
    function GetChildren : IInterfaceList;
    function GetModelContainer : IModelContainer;
    function GetGraphLocation : IGraphLocation;
    function GetGraphData : IGraphData;
    function GetParent : IGraphLocation;
    function GetProject : ICustomProjectGroupProject;
    procedure SetChildren(const Value : IInterfaceList);
    function GetFileName : string;
    procedure SetFileName(const Value : string);

  public
    class function CreateNewContainer(const parentContainer : TProjectTreeContainer; const displayName : string; const identifier : string; const childId : string = '') : TProjectTreeContainer;

    property FileName : string read GetFileName write SetFileName;
    property ModelContainer : IModelContainer read GetModelContainer;
    property Parent : IGraphLocation read GetParent;
    property Project : ICustomProjectGroupProject read GetProject;
    property ImageIndex : Integer read GetImageIndex write SetImageIndex;
    property DisplayName : string read GetDisplayName;
    property Children : IInterfaceList read GetChildren write SetChildren;
    property GraphLocation : IGraphLocation read GetGraphLocation;
    property GraphData : IGraphData read GetGraphData;
  end;



implementation

uses
  System.SysUtils,
  WinApi.Windows;

const
  //used GExperts PE Information to find all the container classes
  //this one looks the most likely - couldn't get TStdContainer to work
  cCategoryContainerClass = 'Containers.TStdContainerCategory';


class constructor TProjectTreeContainer.Create;
var
  rttiMethod : TRttiMethod;
begin
  FRttiContext := TRttiContext.Create;
  FContainerConstructor := nil;
  //find the class we will use to create new containers.. not 100% on correct class to use here.
  FCategoryRttiType := FRttiContext.FindType(cCategoryContainerClass).AsInstance;
  Assert(FCategoryRttiType <> nil);
  //find the constructor and cache it for later.
  for rttiMethod in FCategoryRttiType.GetMethods do
  begin
    if rttiMethod.IsConstructor then
    begin
      FContainerConstructor := rttiMethod;
      exit;
    end;
  end;
end;

class function TProjectTreeContainer.CreateNewContainer(const parentContainer : TProjectTreeContainer; const displayName, identifier, childId : string) : TProjectTreeContainer;
var
  params : TArray<TRttiParameter>;
  locationParam, projectParam, modelParam : TValue;
  model : IInterface; //note this can't be more specific
  project : IInterface;
  graphLocation : IInterface;
begin
  Assert(FContainerConstructor <> nil);
  params := FContainerConstructor.GetParameters;

  //from GExperts PE information on XE7
  //@Containers@TStdContainerCategory@$bctr$qqrx54System@%DelphiInterface$27Projectintf@IModelContainer%x61System@%DelphiInterface$34Codemgr@ICustomProjectGroupProject%x50System@%DelphiInterface$23Idemodel@IGraphLocation%x20System@UnicodeStringt4t4

  // GetParameters gives more information about the parameters
  //
  // 0 : AModelContainer : IModelContainer
  // 1 : AProject : ICustomProjectGroupProject
  // 2 : AGraphParent : IGraphLocation // parent graph node
  // 3 : ACategoryName : string  // display name
  // 4 : ACategoryIdent : string // tools api ident is typically just a unique string
  // 5 : AChildItemID : string //dunno what this is meant to be but seems to work with ''

  model := parentContainer.ModelContainer;

  TValue.Make(@model, params[0].ParamType.Handle, modelParam);

  project := parentContainer.Project;
  TValue.Make(@project, params[1].ParamType.Handle, projectParam);

  graphLocation := TInterfacedObject(parentContainer) as IGraphLocation;
  TValue.Make(@graphLocation, params[2].ParamType.Handle, locationParam);


  Result := TProjectTreeContainer(FContainerConstructor.Invoke(FCategoryRttiType.MetaclassType, [modelParam, projectParam, locationParam, displayName, identifier, childId]).AsObject);


end;

function TProjectTreeContainer.GetChildren : IInterfaceList;
begin
  Result := IInterfaceList(FRttiContext.GetType(ClassType).GetField('FChildren').GetValue(Self).AsInterface);
end;

function TProjectTreeContainer.GetDisplayName : string;
begin
  Result := FRttiContext.GetType(ClassType).GetField('FDisplayName').GetValue(Self).AsString;
end;

function TProjectTreeContainer.GetFileName : string;
begin
  Result := FRttiContext.GetType(ClassType).GetField('FFileName').GetValue(Self).AsString;
end;

function TProjectTreeContainer.GetImageIndex : Integer;
begin
  Result := FRttiContext.GetType(ClassType).GetField('FImageIndex').GetValue(Self).AsInteger;
end;

function TProjectTreeContainer.GetModelContainer : IModelContainer;
begin
  //  Result := FRttiContext.GetType(ClassType).GetField('FModelContainer').GetValue(Self).AsType<IModelContainer>;  //gets typecast error
  Result := IModelContainer(FRttiContext.GetType(ClassType).GetField('FModelContainer').GetValue(Self).AsInterface);
end;

function TProjectTreeContainer.GetGraphLocation : IGraphLocation;
begin
  result := TInterfacedObject(Self) as IGraphLocation;
end;

function TProjectTreeContainer.GetGraphData : IGraphData;
begin
  result := TInterfacedObject(Self) as IGraphData;
end;

function TProjectTreeContainer.GetParent : IGraphLocation;
begin
  Result := IGraphLocation(FRttiContext.GetType(ClassType).GetField('FGraphParent').GetValue(Self).AsInterface);
end;

function TProjectTreeContainer.GetProject : ICustomProjectGroupProject;
begin
  Result := ICustomProjectGroupProject(FRttiContext.GetType(ClassType).GetField('FProject').GetValue(Self).AsInterface);
end;

procedure TProjectTreeContainer.SetChildren(const Value : IInterfaceList);
begin
  FRttiContext.GetType(ClassType).GetField('FChildren').SetValue(Self, TValue.From(Value));
end;

procedure TProjectTreeContainer.SetFileName(const Value : string);
begin
  FRttiContext.GetType(ClassType).GetField('FFileName').SetValue(Self, TValue.From(Value));
end;

procedure TProjectTreeContainer.SetImageIndex(const Value : Integer);
begin
  FRttiContext.GetType(ClassType).GetField('FImageIndex').SetValue(Self, Value);
end;


end.

