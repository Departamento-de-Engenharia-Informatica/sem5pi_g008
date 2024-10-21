using Microsoft.EntityFrameworkCore;
using Sempi5.Domain;
using Sempi5.Domain.Patient;
using Sempi5.Domain.PersonalData;
using Sempi5.Domain.Staff;
using Sempi5.Domain.TodoItem;
using Sempi5.Domain.User;
using Sempi5.Infrastructure.PatientRepository;
using Sempi5.Infrastructure.PersonRepository;
using Sempi5.Infrastructure.StaffRepository;
using Sempi5.Infrastructure.TodoItemRepository;
using Sempi5.Infrastructure.UserRepository;

namespace Sempi5.Infrastructure.Databases
{
    public class DBContext : DbContext
    {
        public DbSet<TodoItem> TodoItems { get; set; }
        public DbSet<Staff> StaffMembers { get; set; }
        public DbSet<SystemUser> Users { get; set; }
        public DbSet<Patient> Patients { get; set; }
        public DbSet<Person> Person { get; set; }
        public DbSet<PatientIdTracker> PatientIdTracker { get; set; }
        public DbSet<StaffIdTracker> StaffIdTracker { get; set; }

        public DBContext(DbContextOptions<DBContext> options)
            : base(options)
        { }

        protected override void OnModelCreating(ModelBuilder modelBuilder)
        {
            base.OnModelCreating(modelBuilder);

            modelBuilder.ApplyConfiguration(new TodoItemEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new StaffEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new UserEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new PatientEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new PatientIdTrackerEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new StaffIdTrackerEntityTypeConfiguration());
            modelBuilder.ApplyConfiguration(new PersonEntityTypeConfiguration());
        }

        protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
        {
            base.OnConfiguring(optionsBuilder);
            optionsBuilder.EnableSensitiveDataLogging();
        }


        //Review this method and all adjacent methods because 
        //this method will most likely need to be updated to
        //not cause concurrency issues
        public override int SaveChanges()
        {
            GeneratePatientIdForNewPatient().Wait();
            GenerateStaffIdForNewStaff().Wait();
            return base.SaveChanges();
        }
        
        public override async Task<int> SaveChangesAsync(CancellationToken cancellationToken = default)
        {
           // await GeneratePatientIdForNewPatient();
           // await GenerateStaffIdForNewStaff();
            return await base.SaveChangesAsync(cancellationToken);
        }

        private async Task GenerateStaffIdForNewStaff()
        {
            var entity = await GetStaffIdTracker();
            
            var newStaff = ChangeTracker.Entries<Staff>()
                .Where(entry => entry.State == EntityState.Added)
                .ToList();

            foreach (var entry in newStaff)
            {
                entry.Entity.Id = new StaffId(entry.Entity.User.Role.Substring(0,1) + GenerateStaffId(entity));
                entity.nextIdToUse++;
            }
        }
        
        private async Task<StaffIdTracker> GetStaffIdTracker(){
            
            var year = DateTime.UtcNow.ToString("yyyy");

            var entity = await StaffIdTracker.SingleOrDefaultAsync(t => t.year == year);

            if (entity == null)
            {
                entity = new StaffIdTracker { year = year, nextIdToUse = 1 };
                await StaffIdTracker.AddAsync(entity);
                return entity;
            }

            return entity;
        }

        private async Task GeneratePatientIdForNewPatient()
        {
            var entity = await GetPatientIdTracker(); 
            
            var newPatients = ChangeTracker.Entries<Patient>()
                .Where(entry => entry.State == EntityState.Added)
                .ToList();

            foreach (var entry in newPatients)
            {
                entry.Entity.Id = new PatientId(GeneratePatientId(entity));
                entity.nextIdToUse++;
            }
        }
        
        private async Task<PatientIdTracker> GetPatientIdTracker()
        {
            var yearMonth = DateTime.UtcNow.ToString("yyyyMM");

            var entity = await PatientIdTracker.SingleOrDefaultAsync(t => t.yearMonth == yearMonth);

            if (entity == null)
            {
                entity = new PatientIdTracker { yearMonth = yearMonth, nextIdToUse = 1 };
                await PatientIdTracker.AddAsync(entity);
                return entity;
            }

            return entity;
        }
        
        private string GeneratePatientId(PatientIdTracker entity)
        {
            var yearMonth = DateTime.UtcNow.ToString("yyyyMM");
            
            return $"{yearMonth}{entity.nextIdToUse:D5}";
        }

        private string GenerateStaffId(StaffIdTracker entity)
        {
            var year = DateTime.UtcNow.ToString("yyyy");

            return $"{year}{entity.nextIdToUse:D5}";
        }
    }
}