using Sempi5.Domain.OperationTypeAggregate;
using Sempi5.Domain.RequiredStaffAggregate;
using Sempi5.Domain.Shared;
using Sempi5.Domain.SpecializationAggregate;

namespace Sempi5.Infrastructure.RequiredStaffAggregate;

public interface IRequiredStaffRepository : IRepository<RequiredStaff, RequiredStaffID>
{
    public Task<RequiredStaff> GetByRequiredStaff(RequiredStaff requiredStaff);
    public Task<RequiredStaff> GetBySpecializationAndOperationId(Specialization specialization, OperationTypeID operationTypeId);
    Task<List<RequiredStaff>> GetAllRequiredAsync();
}