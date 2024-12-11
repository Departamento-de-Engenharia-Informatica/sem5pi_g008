namespace Sempi5.Domain.SpecializationAggregate;

public class SpecializationMap
{
    
    public static SpecializationDTO toDTO(Specialization specialization)
    {
        return new SpecializationDTO
        {
            specializationID = specialization.Id.AsLong(),
            specializationName = specialization.specializationName.ToString()
            
        };
    }
    
    public static Specialization toEntity(SpecializationDTO specializationDTO)
    {
        return new Specialization(new SpecializationName(specializationDTO.specializationName));
    } 
}