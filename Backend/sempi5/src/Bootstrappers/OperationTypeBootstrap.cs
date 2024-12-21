using Sempi5.Domain.OperationTypeAggregate;
using Sempi5.Domain.RequiredStaffAggregate;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Infrastructure.OperationTypeAggregate;
using OperationType = Microsoft.OpenApi.Models.OperationType;

namespace Sempi5.Bootstrappers;

public class OperationTypeBootstrap
{
    private readonly IOperationTypeRepository _operationTypeRepository;
    
    public OperationTypeBootstrap(IOperationTypeRepository operationTypeRepository)
    {
        _operationTypeRepository = operationTypeRepository;
    }
    
    public async Task SeedOperationTypes()
    {
        var operationType1 = new Domain.OperationTypeAggregate.OperationType
        (
            new OperationName("Heart Surgery Simple"),
            new List<RequiredStaff>
            {
                new RequiredStaff(new NumberOfStaff(1), new Specialization(new SpecializationName("Cardiologist 1"), new SpecializationCode("C2"), new SpecializationDescription("Cardiologist specialization"))),
                new RequiredStaff(new NumberOfStaff(3), new Specialization(new SpecializationName("Nurse 1"), new SpecializationCode("N2"), new SpecializationDescription("Nurse specialization"))),
                new RequiredStaff(new NumberOfStaff(2), new Specialization(new SpecializationName("Surgeon 1"), new SpecializationCode("S1"), new SpecializationDescription("Surgeon specialization")))
            },
            TimeSpan.FromHours(3),
            TimeSpan.FromHours(4),
            TimeSpan.FromMinutes(45)
        );
        
        var operationType2 = new Domain.OperationTypeAggregate.OperationType
        (
            new OperationName("Knee Surgery"),
            new List<RequiredStaff>
            {
                new RequiredStaff(new NumberOfStaff(2), new Specialization(new SpecializationName("Nurse 2"), new SpecializationCode("N3"), new SpecializationDescription("Nurse specialization"))),
                new RequiredStaff(new NumberOfStaff(1), new Specialization(new SpecializationName("Surgeon 2"), new SpecializationCode("S2"), new SpecializationDescription("Surgeon specialization")))
            },
            TimeSpan.FromHours(1),
            TimeSpan.FromHours(2),
            TimeSpan.FromMinutes(30)
        );
        
        await _operationTypeRepository.AddAsync(operationType1);
        await _operationTypeRepository.AddAsync(operationType2);
    }
}