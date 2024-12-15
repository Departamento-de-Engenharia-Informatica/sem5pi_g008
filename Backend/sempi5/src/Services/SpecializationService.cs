using Sempi5.Domain.Shared;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Domain.SpecializationAggregate.SpecializationExceptions;
using Sempi5.Infrastructure.SpecializationAggregate;

namespace Sempi5.Services;

public class SpecializationService
{
    private readonly ISpecializationRepository _specializationRepository;
    private readonly IUnitOfWork _unitOfWork;

    public SpecializationService(ISpecializationRepository specializationRepository, IUnitOfWork unitOfWork)
    {
        _specializationRepository = specializationRepository;
        _unitOfWork = unitOfWork;
    }
    
    public async Task<SpecializationDTO> SpecializationByName(string specializationName)
    {
        
        var specializationNameObj = new SpecializationName(specializationName);
        
        var specialization = await _specializationRepository.GetActiveBySpecializationName(new Specialization(specializationNameObj));
        
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
    
    public async Task DeleteSpecialization(string specializationName)
    {
        
        var specialization = await _specializationRepository.GetActiveBySpecializationName(new Specialization(new SpecializationName(specializationName)));

        if (specialization == null) throw new SpecializationNotFoundException("Specialization not found.");
        
        specialization.specializationStatus = SpecializationStatusEnum.INACTIVE;

        await _unitOfWork.CommitAsync();
    }
    
    public async Task<SpecializationDTO> CreateSpecialization(string specializationName)
    {
        
        var specialization = new Specialization(new SpecializationName(specializationName));
        
        var spec = await _specializationRepository.GetBySpecializationName(specialization);
        
        if(spec != null)
        {
            throw new SpecializationInUseException("Specialization already exists.");
        }
        
        await _specializationRepository.AddAsync(specialization);
        
        await _unitOfWork.CommitAsync();
        
        return SpecializationMap.toDTO(specialization);
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