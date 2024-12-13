using Microsoft.AspNetCore.Mvc;
using Sempi5.Domain.OperationTypeAggregate;
using Sempi5.Domain.OperationTypeAggregate.DTOs;
using Sempi5.Domain.PatientAggregate;
using Sempi5.Domain.RequiredStaffAggregate;
using Sempi5.Domain.RequiredStaffAggregate.DTOs;
using Sempi5.Domain.Shared;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Infrastructure.OperationTypeAggregate;
using Sempi5.Infrastructure.OperationTypeRepository;
using Sempi5.Infrastructure.RequiredStaffAggregate;
using Sempi5.Infrastructure.RequiredStaffRepository;
using Sempi5.Infrastructure.SpecializationAggregate;
using Sempi5.Infrastructure.SpecializationRepository;

namespace Sempi5.Services;

public class OperationTypeService
{
    private readonly IOperationTypeRepository _operationTypeRepository;
    private readonly ISpecializationRepository _specializationRepository;
    private readonly IRequiredStaffRepository _requiredStaffRepository;
    private readonly IUnitOfWork _unitOfWork;

    public OperationTypeService(IOperationTypeRepository operationTypeRepository,
        ISpecializationRepository specializationRepository, IRequiredStaffRepository requiredStaffRepository,
        IUnitOfWork unitOfWork)
    {
        _operationTypeRepository = operationTypeRepository;
        _specializationRepository = specializationRepository;
        _requiredStaffRepository = requiredStaffRepository;
        _unitOfWork = unitOfWork;
    }

    public async Task AddNewOperationType(OperationTypeDTO dto)
    {
        var operationType = await OperationTypeDtoToObject(dto);
        await _operationTypeRepository.AddAsync(operationType);
        await _unitOfWork.CommitAsync();
    }

    public async Task<OperationType> OperationTypeDtoToObject(OperationTypeDTO dto)
    {
        var operationName = new OperationName(dto.OperationName);
        var setupDuration = TimeSpan.Parse(dto.SetupDuration);
        var surgeryDuration = TimeSpan.Parse(dto.SurgeryDuration);
        var cleaningDuration = TimeSpan.Parse(dto.CleaningDuration);

        var operationType = new OperationType(operationName, setupDuration, surgeryDuration, cleaningDuration);

        foreach (var requiredStaffDto in dto.RequiredStaff)
        {
            var requiredStaff = RequiredStaffDtoToObject(requiredStaffDto);
            var specialization = await _specializationRepository.GetBySpecializationName(requiredStaff.Specialization);
            if (specialization != null)
            {
                requiredStaff.Specialization = specialization;
            }

            operationType.AddRequiredStaff(requiredStaff);
        }

        return operationType;
    }

    public RequiredStaff RequiredStaffDtoToObject(RequiredStaffDTO dto)
    {
        var specialization = new Specialization(new SpecializationName(dto.Specialization));
        var numberOfStaff = new NumberOfStaff(dto.NumberOfStaff);
        var requiredStaff = new RequiredStaff(numberOfStaff, specialization);
        return requiredStaff;
    }

    public async Task DeleteOperationType(string operationName)
    {
        OperationName name = new OperationName(operationName);
        var operationType = await _operationTypeRepository.GetOperationTypeByName(name);
        if (operationType == null)
        {
            throw new ArgumentException("Operation type not found.");
        }

        operationType.MarkAsNoLongerPerformed();
        await _unitOfWork.CommitAsync();
    }

    public async Task EditOperationTypeName(string oldOperationName, string newOperationName)
    {
        var oldName = new OperationName(oldOperationName);
        var newName = new OperationName(newOperationName);
        var operationType = await _operationTypeRepository.GetOperationTypeByName(oldName);
        if (operationType == null)
        {
            throw new ArgumentException("Operation type not found.");
        }

        operationType.Name = newName;
        await _unitOfWork.CommitAsync();
    }

    public async Task AddRequiredStaffToOperationType(string operationTypeName, RequiredStaffDTO requiredStaffDto)
    {
        var operationName = new OperationName(operationTypeName);
        var operationType = await _operationTypeRepository.GetOperationTypeByName(operationName);
        if (operationType == null)
        {
            throw new ArgumentException("Operation type not found.");
        }

        var requiredStaff = RequiredStaffDtoToObject(requiredStaffDto);
        var specialization = await _specializationRepository.GetBySpecializationName(requiredStaff.Specialization);
        if (specialization != null)
        {
            requiredStaff.Specialization = specialization;
        }

        operationType.AddRequiredStaff(requiredStaff);
        await _unitOfWork.CommitAsync();
    }

    public async Task RemoveRequiredStaffFromOperationType(string operationTypeName, string specializationName)
    {
        var operationName = new OperationName(operationTypeName);
        var operationType = await _operationTypeRepository.GetOperationTypeByName(operationName);
        if (operationType == null)
        {
            throw new ArgumentException("Operation type not found.");
        }

        var requiredStaff = GetRequiredStaffBySpecializationFromOperationType(specializationName, operationType);
        if (requiredStaff == null)
        {
            throw new ArgumentException("Required staff not found.");
        }

        operationType.RemoveRequiredStaff(requiredStaff);
        await _unitOfWork.CommitAsync();
    }

    private RequiredStaff GetRequiredStaffBySpecializationFromOperationType(string specializationName,
        OperationType operationType)
    {
        foreach (var requiredStaff in operationType.RequiredStaff)
        {
            if (requiredStaff.Specialization.specializationName.ToString().Equals(specializationName))
            {
                return requiredStaff;
            }
        }

        return null;
    }

    public async Task<OperationType> EditOperationTypeSetupDuration(string operationName, string setupDuration)
    {
        var name = new OperationName(operationName);
        var operationType = await _operationTypeRepository.GetOperationTypeByName(name);
        if (operationType == null)
        {
            throw new ArgumentException("Operation type not found.");
        }

        operationType.SetupDuration = TimeSpan.Parse(setupDuration);
        await _unitOfWork.CommitAsync();
        return operationType;
    }

    public async Task<OperationType> EditOperationTypeSurgeryDuration(string operationName, string surgeryDuration)
    {
        var name = new OperationName(operationName);
        var operationType = await _operationTypeRepository.GetOperationTypeByName(name);
        if (operationType == null)
        {
            throw new ArgumentException("Operation type not found.");
        }

        operationType.SurgeryDuration = TimeSpan.Parse(surgeryDuration);
        await _unitOfWork.CommitAsync();
        return operationType;
    }

    public async Task<OperationType> EditOperationTypeCleaningDuration(string operationName, string cleaningDuration)
    {
        var name = new OperationName(operationName);
        var operationType = await _operationTypeRepository.GetOperationTypeByName(name);
        if (operationType == null)
        {
            throw new ArgumentException("Operation type not found.");
        }

        operationType.CleaningDuration = TimeSpan.Parse(cleaningDuration);
        await _unitOfWork.CommitAsync();
        return operationType;
    }

    public async Task<List<OperationTypeDTO>> ListOperationTypeByName(string operationName)
    {
        var operationType =
            await _operationTypeRepository.GetOperationTypeListByName(new OperationName(operationName));

        if (operationType == null)
        {
            throw new ArgumentException("Operation type not found");
        }

        return createListOfOperationTypeDTOs(operationType);
    }

    public async Task<List<OperationTypeDTO>> ListOperationTypeBySpecialization(string specializationName)
    {
        var operationType =
            await _operationTypeRepository.GetOperationTypeListBySpecialization(
                new SpecializationName(specializationName));

        if (operationType == null)
        {
            throw new ArgumentException("Operation type not found");
        }

        return createListOfOperationTypeDTOs(operationType);
    }

    public async Task<List<OperationTypeDTO>> ListOperationTypeByStatus(bool status)
    {
        var operationType = await _operationTypeRepository.GetOperationTypeListByStatus(status);

        if (operationType == null)
        {
            throw new ArgumentException("Operation type not found");
        }

        return createListOfOperationTypeDTOs(operationType);
    }

    private List<SearchedOperationTypeDTO> buildSearchedOperationTypeDtoList(IEnumerable<OperationType> operationTypes)
    {
        List<SearchedOperationTypeDTO> searchedOperationTypeDtoList = new List<SearchedOperationTypeDTO>();

        foreach (var operationType in operationTypes)
        {
            searchedOperationTypeDtoList.Add(operationTypeToSearchedOperationTypeDto(operationType));
        }

        return searchedOperationTypeDtoList;
    }

    private SearchedOperationTypeDTO operationTypeToSearchedOperationTypeDto(OperationType operationType)
    {
        if (operationType == null)
        {
            throw new ArgumentException("Operation type not found");
        }

        return new SearchedOperationTypeDTO
        {
            id = operationType.Id.AsString(),
            name = operationType.Name.ToString(),
            requiredStaff = operationType.RequiredStaff.ToString(),
            estimatedDuration = (operationType.SetupDuration.Duration() + operationType.CleaningDuration.Duration() +
                                 operationType.SurgeryDuration.Duration()).ToString()
        };
    }

    public async Task<List<OperationTypeDTO>> ListOperationTypes()
    {
        var operationTypes = await _operationTypeRepository.GetAllAsyncWithIncludes();
        return createListOfOperationTypeDTOs(operationTypes);
    }

    private List<OperationTypeDTO> createListOfOperationTypeDTOs(List<OperationType> operationTypes)
    {
        var operationTypeDTOs = new List<OperationTypeDTO>();

        foreach (var operationType in operationTypes)
        {
            operationTypeDTOs.Add(operationTypeToOperationTypeDTO(operationType));
        }

        return operationTypeDTOs;
    }

    private OperationTypeDTO operationTypeToOperationTypeDTO(OperationType operationType)
    {
        var requiredStaffDTOs = new List<RequiredStaffDTO>();
        
        foreach (var requiredStaff in operationType.RequiredStaff)
        {
            requiredStaffDTOs.Add(new RequiredStaffDTO
            {
                NumberOfStaff = requiredStaff.NumberOfStaff.value,
                Specialization = requiredStaff.Specialization.specializationName.ToString()
            });
        }

        return new OperationTypeDTO
        {
            OperationName = operationType.Name.ToString(),
            SetupDuration = operationType.SetupDuration.ToString(),
            SurgeryDuration = operationType.SurgeryDuration.ToString(),
            CleaningDuration = operationType.CleaningDuration.ToString(),
            RequiredStaff = requiredStaffDTOs
        };
    }

    public  async Task<List<OperationType>> getOperationType()
    {
        return await _operationTypeRepository.GetAllOperationTypeAsync();
        
    }
    public async Task<List<RequiredStaff>> getRequiredStaff()
    {
        return await _requiredStaffRepository.GetAllRequiredAsync();
    }
}