using Sempi5.Domain.Shared;

namespace Sempi5.Domain.SpecializationAggregate;

public class Specialization : Entity<SpecializationID>, IAggregateRoot
{
    public SpecializationID Id { get; set; }
    
    public virtual SpecializationName specializationName { get;}
    
    public  SpecializationStatusEnum specializationStatus { get; set; }

    private Specialization() { }
    
    public Specialization(SpecializationName specializationName)
    {
        
        this.specializationName = specializationName;
        specializationStatus = SpecializationStatusEnum.ACTIVE;
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