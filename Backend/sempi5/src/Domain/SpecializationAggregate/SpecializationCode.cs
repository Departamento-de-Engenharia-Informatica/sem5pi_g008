namespace Sempi5.Domain.SpecializationAggregate;

public class SpecializationCode
{
    private SpecializationCode() {}
    
    private string code { get; set; }
    
    public SpecializationCode(string code)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(code);
        
        this.code = code;
    }
    
    public override string ToString()
    {
        return code;
    }
    
    public bool Equals(SpecializationCode specializationCode)
    {
        if(specializationCode == null)
        {
            return false;
        }
        
        return specializationCode.code == code;
    }

    
    
}