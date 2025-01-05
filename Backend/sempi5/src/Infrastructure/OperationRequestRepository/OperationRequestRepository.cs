using Microsoft.EntityFrameworkCore;
using Sempi5.Domain.OperationRequestAggregate;
using Sempi5.Domain.OperationTypeAggregate;
using Sempi5.Domain.Shared;
using Sempi5.Domain.StaffAggregate;
using Sempi5.Infrastructure.Databases;
using Sempi5.Infrastructure.OperationRequestAggregate;
using Sempi5.Infrastructure.Shared;

namespace Sempi5.Infrastructure.OperationRequestRepository;

public class OperationRequestRepository : BaseRepository<OperationRequest, OperationRequestID>,
    IOperationRequestRepository
{
    private readonly DBContext context;

    public OperationRequestRepository(DBContext dbContext) : base(dbContext.OperationRequests)
    {
        this.context = dbContext;
    }

    public async Task<OperationRequest> GetOperationRequestById(string id)
    {
        if (string.IsNullOrEmpty(id))
        {
            return null;
        }
        
        var operationRequest=context.OperationRequests
            .Include(r=>r.Doctor)
            .AsEnumerable()
            .FirstOrDefault(o=>o.Id.AsString().Equals(id));

        return operationRequest;
    }
    public async Task<List<OperationRequest>> GetallOperationRequest()
    {
        return await context.OperationRequests
            .Include(o=>o.Doctor)
            .Include(o=>o.Doctor).ThenInclude(o=>o.Person)
            .Include(o=>o.Doctor).ThenInclude(o=>o.User)
            .Include(r => r.Patient)
            .Include(o=>o.Patient).ThenInclude(o=>o.Person)
            .Include(r => r.Patient).ThenInclude(o=>o.User)
            .Include(r => r.OperationType)
            .ToListAsync();
    }

    public async Task<OperationRequest> GetByIdAsync(OperationRequestID id)
    {
        return await context.OperationRequests.FindAsync(id);
    }

    public async Task<List<OperationRequest>> GetAllAsync()
    {
        return await context.OperationRequests.ToListAsync();
    }

    public async Task AddAsync(OperationRequest request)
    {
        await context.OperationRequests.AddAsync(request);
        await context.SaveChangesAsync();
    }

    public async Task RemoveAsync(OperationRequest request)
    {
        context.OperationRequests.Remove(request);
        await context.SaveChangesAsync();
    }

    public async Task<List<OperationRequest>> SearchAsync(string patientName, string type,
        string priority )
    {
        var query = context.OperationRequests
            .Include(r => r.Patient)
            .Include(r => r.Patient).ThenInclude(o=>o.Person)
            .Include(r => r.Patient).ThenInclude(o=>o.User)
            .Include(r => r.OperationType)
            .AsQueryable();
        
        if (!string.IsNullOrEmpty(patientName))
        {
            query = query.Where(r => r.Patient.Person != null && r.Patient.Person.FullName.Equals(new Name(patientName)));
        }
        
        if (!string.IsNullOrEmpty(type))
        {
            query = query.Where(r => r.OperationType.Name.Equals(new OperationName(type)));
        }
        
        if (!string.IsNullOrEmpty(priority) && Enum.TryParse<PriorityEnum>(priority, true, out var parsedPriority))
        {
            query = query.Where(r => r.PriorityEnum == parsedPriority);
        }
        
        // Executa a consulta e retorna os resultados
        return await query.ToListAsync();
    }
    
    public async Task<List<OperationRequest>> SearchAsyncWithDoctocId(string patientName, string type,
        string priority ,string doctorId)
    {
        var query = context.OperationRequests
            .Include(r => r.Patient)
            .Include(r => r.Patient).ThenInclude(o=>o.Person)
            .Include(r => r.Patient).ThenInclude(o=>o.User)
            .Include(r => r.OperationType)
            .Include(r=>r.Doctor)
            .AsQueryable();
        
        if (!string.IsNullOrEmpty(patientName))
        {
            query = query.Where(r => r.Patient.Person != null && r.Patient.Person.FullName.Equals(new Name(patientName)));
        }
        
        if (!string.IsNullOrEmpty(type))
        {
            query = query.Where(r => r.OperationType.Name.Equals(new OperationName(type)));
        }
        
        if (!string.IsNullOrEmpty(priority) && Enum.TryParse<PriorityEnum>(priority, true, out var parsedPriority))
        {
            query = query.Where(r => r.PriorityEnum == parsedPriority);
        }
        query = query.Where(r => r.Doctor.Id.Equals(new StaffId(doctorId)));
        return await query.ToListAsync();
    }

    public async Task<OperationRequest?> GetByDoctorId(string doctorId)
    {
        OperationRequest request;
        request =  context.OperationRequests.AsEnumerable().FirstOrDefault(r => r.Doctor.Id.AsString().Equals(doctorId));
        return request;
    }


    public async Task UpdateAsync(OperationRequest request)
    {
        context.OperationRequests.Update(request);
        await context.SaveChangesAsync();
    }

    public async Task DeleteAsync(OperationRequestID id)
    {
        var request = await GetByIdAsync(id);
        if (request != null)
        {
            context.OperationRequests.Remove(request);
            await context.SaveChangesAsync();
        }
    }

    public async Task<List<OperationRequest>> GetDoctorsOperationsRequests(string doctorId)
    {
        return await context.OperationRequests
            .Include(o => o.OperationType.Name)
            .Include(o => o.Patient.Person.FullName)
            .Include(o => o.DeadLineDate)
            .Include(o => o.PriorityEnum)
            .Where(o => o.Doctor.Id.Equals(doctorId))
            .ToListAsync();
    }
    
}