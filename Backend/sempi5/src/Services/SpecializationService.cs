using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Domain.SpecializationAggregate.SpecializationExceptions;
using Sempi5.Infrastructure.SpecializationAggregate;

namespace Sempi5.Services;

public class SpecializationService
{
    private readonly ISpecializationRepository _specializationRepository;
    
    public SpecializationService(ISpecializationRepository specializationRepository)
    {
        _specializationRepository = specializationRepository;
    }
    
    public async Task<SpecializationDTO> SpecializationByName(string specializationName)
    {
        
        
        var specializationNameObj = new SpecializationName(specializationName);
        
        Console.WriteLine("\n\n\n" + specializationNameObj + "\n\n\n");
        
        var specialization = await _specializationRepository.GetBySpecializationName(new Specialization(specializationNameObj));
        
        if(specialization == null)
        {
            throw new SpecializationNotFoundException("Specialization not found.");
        }
        
        return SpecializationMap.toDTO(specialization);
    }
    
    public async Task<List<SpecializationDTO>> ListAllSpecializations()
    {
        
        var specializationList = await _specializationRepository.GetAllActiveSpecializations();
        
        if(specializationList.Count == 0)
        {
            throw new NoSpecializationsFoundException("No specializations found.");
        }
        
        var specializationsDtoList = MapSpecializationListToSpecializationDTOList(specializationList);
        
        return specializationsDtoList;
    }
    
    private List<SpecializationDTO> MapSpecializationListToSpecializationDTOList(List<Specialization> specializations)
    {
        List<SpecializationDTO> specializationDTOs = new List<SpecializationDTO>();
        
        foreach(Specialization specialization in specializations)
        {
            specializationDTOs.Add(SpecializationMap.toDTO(specialization));
        }
        
        return specializationDTOs;
    }
    
}