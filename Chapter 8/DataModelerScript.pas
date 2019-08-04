procedure OnClassGenerated(Args: TClassGeneratedArgs);      
var
  field: TCodeTypeMember;
  attr: TCodeAttributeDeclaration;
  i: integer;
begin
  if Args.CodeType.Name = 'TAgent' then     
  begin 
    // Add the Base class       
    Args.CodeType.BaseType:=TCodeTypeReference.Create('TBase');   
    
    // Remove the ID Field
    for i:=Args.CodeType.Members.Count - 1 downto 0  do
    begin                                                  
      field:=Args.CodeType.Members.Items[i];   
      if field.Name='FID' then
      begin                              
        Args.CodeType.Members.Delete(i);
        break;
      end;                             
    end;      
                        
    // Remove the ID property  
    for i:=Args.CodeType.Members.Count - 1 downto 0 do                             
    begin                                                  
      field:=Args.CodeType.Members.Items[i];   
      if field.Name='ID' then
      begin            
        Args.CodeType.Members.Delete(i);
        break;                                      
      end;        
    end;    
    
    // Remove the ID Attribute
    for i:=Args.CodeType.CustomAttributes.Count - 1 downto 0 do
    begin
      attr:=Args.CodeType.CustomAttributes.Items[i];   
      if attr.Name='Id' then
      begin                   
        Args.CodeType.CustomAttributes.Delete(i);
        break;
      end;                                
    end;          

  end;
  if Args.CodeType.Name = 'TBase' then     
  begin   
    Args.CodeType.AddAttribute('Inheritance').AddRawArgument('TInheritanceStrategy.JoinedTables'); 
  end;
end;                                                                     