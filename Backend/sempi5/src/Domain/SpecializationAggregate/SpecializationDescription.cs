namespace Sempi5.Domain.SpecializationAggregate;

public class SpecializationDescription
{
    private string description { get; set; }
    
    private SpecializationDescription() {}
    
    public SpecializationDescription(string description)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(description);
        
        this.description = description;
    }
    
    public override string ToString()
    {
        return description;
    }
    
}