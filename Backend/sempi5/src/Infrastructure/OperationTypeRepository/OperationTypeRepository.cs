using Microsoft.EntityFrameworkCore;
using Sempi5.Domain.OperationRequestAggregate;
using Sempi5.Domain.OperationTypeAggregate;
using Sempi5.Domain.OperationTypeAggregate.DTOs;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Infrastructure.Databases;
using Sempi5.Infrastructure.OperationTypeAggregate;
using Sempi5.Infrastructure.Shared;

namespace Sempi5.Infrastructure.OperationTypeRepository;

public class OperationTypeRepository : BaseRepository<OperationType, OperationTypeID>, IOperationTypeRepository
{
    private readonly DBContext context;
    
    public OperationTypeRepository(DBContext dbContext) : base(dbContext.OperationTypes)
    {
        this.context = dbContext;
    }
    
    public async Task<OperationType> GetOperationTypeByName(OperationName name)
    {
        return await context.OperationTypes
            .Include(x => x.RequiredStaff)
            .ThenInclude(r => r.Specialization)
            .FirstOrDefaultAsync(x => x.Name.Equals(name));
    }

    public async Task<List<OperationType>> GetOperationTypeListByName(OperationName name)
    {

        return await context.OperationTypes
            .Include(o => o.RequiredStaff)
            .ThenInclude(r => r.Specialization)
            .Where(o => o.Name.Equals(name))
            .ToListAsync();
    }

    public async Task<List<OperationType>> GetOperationTypeListBySpecialization(SpecializationName specializationName)
    {

        var operationTypes = await context.OperationTypes
            .Include(o => o.RequiredStaff)
            .ThenInclude(r => r.Specialization)
            .ToListAsync();

        return operationTypes
            .Where(o => o.RequiredStaff.Any(r => r.Specialization.specializationName.Equals(specializationName)))
            .ToList();
    }

    public async Task<List<OperationType>> GetOperationTypeListByStatus(bool status)
    {
        return await context.OperationTypes
            .Include(o => o.RequiredStaff)
            .ThenInclude(r => r.Specialization)
            .Where(o => o.stillPerformed.Equals(status))
            .ToListAsync();
    }
    
    public async Task<List<OperationType>> GetAllAsyncWithIncludes()
    {
        return await context.OperationTypes
            .Include(o => o.RequiredStaff)
            .ThenInclude(r => r.Specialization)
            .Where(o => o.stillPerformed)
            .ToListAsync();
    }

    public async Task<List<OperationType>> GetAllOperationTypeAsync()
    {
        return await context.OperationTypes
            .Include(o => o.RequiredStaff)
            .ThenInclude(r => r.Specialization)
            .ToListAsync();
    }
}
