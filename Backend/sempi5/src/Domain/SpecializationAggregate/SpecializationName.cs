using Sempi5.Domain.Shared;
using Sempi5.Domain.SpecializationAggregate.SpecializationExceptions;

namespace Sempi5.Domain.SpecializationAggregate;

public class SpecializationName : IValueObject
{
    public virtual string name { get; set; }

    private SpecializationName() {}
    
    public SpecializationName(string name)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(name);
        
        this.name = name;
    }

    public override string ToString()
    {
        return name;
    }
    
    public bool Equals(SpecializationName specializationName)
    {
        if(specializationName == null)
        {
            return false;
        }
        
        return specializationName.name == name;
    }
    
    
    
    
    
}