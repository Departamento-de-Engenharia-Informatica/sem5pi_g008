using Sempi5.Domain.OperationRequestAggregate;
using Sempi5.Domain.OperationRequestAggregate.DTOs;
using Sempi5.Domain.OperationTypeAggregate;
using Sempi5.Domain.PatientAggregate;
using Sempi5.Domain.RequiredStaffAggregate;
using Sempi5.Domain.Shared;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Domain.StaffAggregate;
using Sempi5.Domain.User;
using Sempi5.Infrastructure.OperationRequestAggregate;
using Sempi5.Infrastructure.OperationRequestRepository;
using Sempi5.Infrastructure.OperationTypeAggregate;
using Sempi5.Infrastructure.OperationTypeRepository;
using Sempi5.Infrastructure.PatientAggregate;
using Sempi5.Infrastructure.PatientRepository;
using Sempi5.Infrastructure.SpecializationAggregate;
using Sempi5.Infrastructure.SpecializationRepository;
using Sempi5.Infrastructure.StaffAggregate;
using Sempi5.Infrastructure.StaffRepository;

namespace Sempi5.Services;

public class OperationRequestService
{
    private readonly IOperationRequestRepository _operationRequestRepository;
    private readonly ISpecializationRepository _specializationRepository;
    private readonly IUnitOfWork _unitOfWork;
    private readonly IStaffRepository _staffRepository;
    private readonly IPatientRepository _patientRepository;
    private readonly IOperationTypeRepository _operationTypeRepository;

    public OperationRequestService(IOperationRequestRepository operationRequestRepository, ISpecializationRepository specializationRepository, IUnitOfWork unitOfWork, IStaffRepository staffRepository, IPatientRepository patientRepository, IOperationTypeRepository operationTypeRepository)
    {
        _operationRequestRepository = operationRequestRepository;
        _specializationRepository = specializationRepository;
        _unitOfWork = unitOfWork;
        _staffRepository = staffRepository;
        _patientRepository = patientRepository;
        _operationTypeRepository = operationTypeRepository;
    }

    public async Task RequestOperation(OperationRequestDTO operationRequestDto)
    {
        var operationRequest = await OperationRequestDtoToObject(operationRequestDto) ;
        await _operationRequestRepository.AddAsync(operationRequest);
        await _unitOfWork.CommitAsync();
    }

    public async Task<OperationRequest> OperationRequestDtoToObject(OperationRequestDTO operationRequestDto)
    {
        
        var patient = await _patientRepository.GetByPatientId(operationRequestDto.patientID);
        var doctor = await _staffRepository.GetActiveStaffById(new StaffId(operationRequestDto.doctorId));
        var operationType = await _operationTypeRepository.GetOperationTypeByName(new OperationName(operationRequestDto.operationType));

        if (ValidateSpecialization(doctor.Specialization, operationType) ==false)
        {
            throw new ArgumentException("Doctor specialization does not match the operation request");
        }
        
        return new OperationRequest(doctor,patient,operationType,DateTime.Parse(operationRequestDto.deadline), (PriorityEnum)Enum.Parse(typeof(PriorityEnum),operationRequestDto.priority.ToUpper()));
    }
    
    private bool ValidateSpecialization(Specialization specialization, OperationType operationType)
    {
        foreach (RequiredStaff requiredStaff in operationType.RequiredStaff)
        {
            if (requiredStaff.Specialization.Equals(specialization))
            {
                return true;
            }
        }
        return false;
    }

    public async Task UpdateOperationRequestDeadline(string operationRequestId, string deadline, string doctor)
    {
        var operationRequest = await _operationRequestRepository.GetOperationRequestById(operationRequestId);
        if (operationRequest == null)
        {
            throw new ArgumentException("Operation request not found");
        }

        await ValidateRequestingDoctor(doctor, operationRequest.Doctor.Id);
        
        operationRequest.DeadLineDate = DateTime.Parse(deadline);
        await _unitOfWork.CommitAsync();
        
    }

    public async Task UpdateOperationRequestPriority(string operationRequestId, string priority, string doctor)
    {
        var operationRequest = await _operationRequestRepository.GetOperationRequestById(operationRequestId);
        if (operationRequest == null)
        {
            throw new ArgumentException("Operation request not found");
        }
        
        await ValidateRequestingDoctor(doctor, operationRequest.Doctor.Id);

        operationRequest.PriorityEnum = (PriorityEnum)Enum.Parse(typeof(PriorityEnum), priority.ToUpper());
        await _unitOfWork.CommitAsync();
        
    }

    private async Task ValidateRequestingDoctor(string doctor, StaffId operationDoctor)
    {
        var doctorR = await _staffRepository.GetActiveStaffById(operationDoctor);
      
        if (doctorR == null)
        {
            throw new ArgumentException("Doctor does not exits");
        }
        
        if (!doctor.Equals(doctorR.User.Email.ToString()))
        {
            throw new ArgumentException("Only the doctor that requested the operation can edit it!");
        }
        
    }

    public async Task<List<OperationRequest>> ListDoctorsOperationRequests(string doctorId)
    {
        var list = await _operationRequestRepository.GetDoctorsOperationsRequests(doctorId);
        if (list.Count == 0)
        {
            throw new ArgumentException("Operation requests not found.");
        }
        
        return list;
    }


    public async Task<List<OperationRequest>> getOperationRequest()
    {
     return await _operationRequestRepository.GetAllAsync();
    }
}