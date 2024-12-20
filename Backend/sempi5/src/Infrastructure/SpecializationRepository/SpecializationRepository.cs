using Microsoft.EntityFrameworkCore;
using Sempi5.Domain.SpecializationAggregate;
using Sempi5.Infrastructure.Databases;
using Sempi5.Infrastructure.Shared;
using Sempi5.Infrastructure.SpecializationAggregate;

namespace Sempi5.Infrastructure.SpecializationRepository
{
    public class SpecializationRepository : BaseRepository<Specialization, SpecializationID>, ISpecializationRepository
    {
        private readonly DBContext context;
        
        public SpecializationRepository(DBContext dbContext) : base(dbContext.Specializations)
        {
            this.context = dbContext;
        }
        
        
        public async Task<Specialization?> GetActiveBySpecializationName(SpecializationName specializationName)
        {
            if(string.IsNullOrWhiteSpace(specializationName.ToString())) {
                return null;
            }
            
            var specializationSearched = await context.Specializations
                .FirstOrDefaultAsync(p => p.specializationName.Equals(specializationName) 
                                          && p.specializationStatus.Equals(SpecializationStatusEnum.ACTIVE)); 
    
            return specializationSearched;        
        }


        public async Task<Specialization?> GetBySpecializationName(SpecializationName specializationName)
        {
            if(string.IsNullOrWhiteSpace(specializationName.ToString())) {
                return null;
            }

            var specializationSearched = await context.Specializations
                .FirstOrDefaultAsync(p => p.specializationName.Equals(specializationName)); 
    
            return specializationSearched;  
        }
        
        public async Task<Specialization?> GetBySpecializationCode(SpecializationCode specializationCode)
        {
            if(string.IsNullOrWhiteSpace(specializationCode.ToString())) {
                return null;
            }

            var specializationSearched = await context.Specializations
                .FirstOrDefaultAsync(p => p.specializationCode.Equals(specializationCode)); 
    
            return specializationSearched;  
        }

        public async Task<List<Specialization>> GetAllActiveSpecializations()
        {
            
            return await context.Specializations
                .Where(p => p.specializationStatus.Equals(SpecializationStatusEnum.ACTIVE))
                .ToListAsync();
        }
    }
    
    
}

