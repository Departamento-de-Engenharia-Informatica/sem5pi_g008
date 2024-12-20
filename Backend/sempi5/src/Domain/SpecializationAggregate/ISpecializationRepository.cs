using Sempi5.Domain.Shared;
using Sempi5.Domain.SpecializationAggregate;

namespace Sempi5.Infrastructure.SpecializationAggregate
{
    public interface ISpecializationRepository : IRepository<Specialization, SpecializationID>
    {
        
        public Task<Specialization?> GetActiveBySpecializationName(SpecializationName specializationName);
        public Task<Specialization?> GetBySpecializationName(SpecializationName specializationName);
        public Task<Specialization?> GetBySpecializationCode(SpecializationCode specializationCode);
        public Task<List<Specialization>> GetAllActiveSpecializations();
        public Task<Specialization?> GetActiveBySpecializationID(SpecializationID specializationID);
    }
}