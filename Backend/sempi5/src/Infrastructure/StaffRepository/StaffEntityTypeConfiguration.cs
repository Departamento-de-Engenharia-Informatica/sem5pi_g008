using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using Sempi5.Domain;
using Sempi5.Domain.StaffAggregate;

namespace Sempi5.Infrastructure.StaffRepository
{
    public class StaffEntityTypeConfiguration : IEntityTypeConfiguration<Staff>
    {
        public void Configure(EntityTypeBuilder<Staff> builder)
        {
            builder.ToTable("Staff");
            builder.HasKey(t => t.Id);

            builder.HasOne(s => s.User)
                .WithOne()
                .HasForeignKey<Staff>("UserId")
                .OnDelete(DeleteBehavior.Cascade)
                .IsRequired(false);

            builder.Property(t => t.Id)
                .HasConversion(
                    v => v.AsString(),
                    v => new StaffId(v)
                )
                .IsRequired()
                .ValueGeneratedOnAdd();

            builder.HasOne(t => t.Person)
                .WithOne()
                .HasForeignKey<Staff>("PersonId")
                .OnDelete(DeleteBehavior.Cascade);

            builder.Property(t => t.LicenseNumber)
                .HasConversion(
                    v => v.licenseNumber(),
                    v => new LicenseNumber(v)
                );
            
            builder.HasIndex(p => p.LicenseNumber)
                .IsUnique();

            builder.HasOne(t => t.Specialization)
                .WithMany()                        // Specialization has many Staff
                .HasForeignKey("SpecializationId")
                .IsRequired();
            
            builder.Property(p => p.Status)
                .IsRequired()
                .HasConversion(
                    p => p.ToString(),
                    p => (StaffStatusEnum)Enum.Parse(typeof(StaffStatusEnum), p)
                );
            builder.HasMany(t=>t.StaffAgendas)
                .WithOne();
            //TODO rever
        }
    }
}