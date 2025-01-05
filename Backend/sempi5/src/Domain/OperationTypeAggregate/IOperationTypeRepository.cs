using Sempi5.Domain.OperationTypeAggregate;
using Sempi5.Domain.OperationTypeAggregate.DTOs;
using Sempi5.Domain.Shared;
using Sempi5.Domain.SpecializationAggregate;

namespace Sempi5.Infrastructure.OperationTypeAggregate;

public interface IOperationTypeRepository : IRepository<OperationType, OperationTypeID>
{
    public Task<OperationType> GetOperationTypeByName(OperationName name);
    public Task<List<OperationType>> GetOperationTypeListByName(OperationName name);
    public Task<List<OperationType>> GetOperationTypeListBySpecialization(SpecializationName specializationName);
    public Task<List<OperationType>> GetOperationTypeListByStatus(bool status);
    public Task<List<OperationType>> GetAllAsyncWithIncludes();

    Task<List<OperationType>> GetAllOperationTypeAsync();
}