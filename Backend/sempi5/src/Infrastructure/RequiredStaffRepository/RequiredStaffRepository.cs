using Microsoft.CodeAnalysis.Elfie.Serialization;
using Microsoft.EntityFrameworkCore;
using Sempi5.Domain.OperationTypeAggregate;
using Sempi5.Domain.RequiredStaffAggregate;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Infrastructure.Databases;
using Sempi5.Infrastructure.RequiredStaffAggregate;
using Sempi5.Infrastructure.Shared;

namespace Sempi5.Infrastructure.RequiredStaffRepository;

public class RequiredStaffRepository : BaseRepository<RequiredStaff, RequiredStaffID>, IRequiredStaffRepository
{
    private readonly DBContext context;

    public RequiredStaffRepository(DBContext dbContext) : base(dbContext.RequiredStaffs)
    {
        this.context = dbContext;
    }

    public Task<RequiredStaff> GetByRequiredStaff(RequiredStaff requiredStaff)
    {
        if (requiredStaff == null)
        {
            return null;
        }

        var name = requiredStaff.Specialization;
        var number = requiredStaff.NumberOfStaff;

        var requiredStaffObj =
            context.RequiredStaffs.FirstOrDefaultAsync(p =>
                p.Specialization.Equals(name) && p.NumberOfStaff.Equals(number));

        return requiredStaffObj;
    }

    public Task<RequiredStaff> GetBySpecializationAndOperationId(Specialization specialization,
        OperationTypeID operationTypeId)
    {
        if (specialization == null || operationTypeId == null)
        {
            return null;
        }

        var requiredStaffObj = context.RequiredStaffs.FirstOrDefaultAsync
        (p => p.Specialization.Equals(specialization) &&
         EF.Property<long>(p, "OperationTypeId") == operationTypeId.AsLong());

        return requiredStaffObj;
    }

    public async Task<List<RequiredStaff>> GetAllRequiredAsync()
    {
        return await context.RequiredStaffs.Include(o=>o.Specialization).ToListAsync();
    }
}