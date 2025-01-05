namespace Sempi5.Domain.SpecializationAggregate;

public class SpecializationMap
{
    
    public static SpecializationDTO toDTO(Specialization specialization)
    {
        return new SpecializationDTO
        {
            specializationID = specialization.Id.AsLong(),
            specializationName = specialization.specializationName.ToString(),
            specializationCode = specialization.specializationCode.ToString(),
            specializationDescription = specialization.specializationDescription.ToString()
        };
    }
    
    public static Specialization toDomain(SpecializationDTO specializationDTO)
    {
        return new Specialization(
            new SpecializationName(specializationDTO.specializationName),
            new SpecializationCode(specializationDTO.specializationCode),
            new SpecializationDescription(specializationDTO.specializationDescription));
    } 
}