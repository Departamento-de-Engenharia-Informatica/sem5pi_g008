using Sempi5.Domain.Shared;

namespace Sempi5.Domain.SpecializationAggregate;

public class Specialization : Entity<SpecializationID>, IAggregateRoot
{
    public SpecializationID Id { get; set; }
    
    public SpecializationName specializationName { get; set; }
    
    public SpecializationStatusEnum specializationStatus { get; set; }

    public SpecializationCode specializationCode { get; set; }
    
    public SpecializationDescription specializationDescription { get; set; }
    
    private Specialization() { }
    
    public Specialization(SpecializationName specializationName, SpecializationCode specializationCode, SpecializationDescription specializationDescription)
    {
        
        this.specializationName = specializationName;
        specializationStatus = SpecializationStatusEnum.ACTIVE;
        this.specializationCode = specializationCode;
        this.specializationDescription = specializationDescription;
    }
    
    public virtual bool Equals(Specialization? obj)
    {
        if (obj is Specialization other)
        {
            return specializationName.ToString() == other.specializationName.ToString();
        }
        return false;
    }
    
}