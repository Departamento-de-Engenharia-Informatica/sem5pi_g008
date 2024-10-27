﻿using Sempi5.Domain.OperationRequestAggregate;
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
        var operationType = await _operationTypeRepository.GetOperationTypeByName(new OperationName(operationRequestDto.operationName));
        var deadline = DateTime.Parse(operationRequestDto.deadline);

        ValidateSpecialization(doctor.Specialization, operationType);
        
        return new OperationRequest(doctor,patient,operationType,deadline,(PriorityEnum)Enum.Parse(typeof(PriorityEnum),operationRequestDto.priority.ToUpper()));
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

    public async Task<OperationRequest> UpdateOperationRequestDeadline(OperationRequestDTO operationRequestDto, string doctor)
    {
        await ValidateRequestingDoctor(doctor, operationRequestDto);

        var operationRequest =
            await _operationRequestRepository.GetOperationRequestById(operationRequestDto.operationRequestId);
        if (operationRequest == null)
        {
            throw new ArgumentException("Operation request not found");
        }

        operationRequest.DeadLineDate = DateTime.Parse(operationRequestDto.deadline);
        await _unitOfWork.CommitAsync();
        
        return operationRequest;
    }

    public async Task<OperationRequest> UpdateOperationRequestPriority(OperationRequestDTO operationRequestDto, string doctor)
    {
        await ValidateRequestingDoctor(doctor, operationRequestDto);
        var operationRequest =
            await _operationRequestRepository.GetOperationRequestById(operationRequestDto.operationRequestId);
        if (operationRequest == null)
        {
            throw new ArgumentException("Operation request not found");
        }
        operationRequest.PriorityEnum =
            (PriorityEnum)Enum.Parse(typeof(PriorityEnum), operationRequestDto.priority.ToUpper());
        await _unitOfWork.CommitAsync();
        
        return operationRequest;
    }

    private async Task<Staff>ValidateRequestingDoctor(string doctor, OperationRequestDTO operationRequestDto)
    {
        var requestingDoctor = await _staffRepository.GetActiveStaffById(new StaffId(operationRequestDto.doctorId));
        if (requestingDoctor == null)
        {
            throw new ArgumentException("Requesting doctor not found");
        }
        if (!requestingDoctor.ToString().Equals(doctor))
        {
            throw new ArgumentException("Only the doctor that requested the operation can edit it!");
        }
        return requestingDoctor;
    }

   
}