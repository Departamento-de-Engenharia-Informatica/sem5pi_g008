using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using Sempi5.Domain.PersonalData;
using Sempi5.Domain.SpecializationAggregate;

namespace Sempi5.Infrastructure.SpecializationRepository;

public class SpecializationEntityTypeConfiguration : IEntityTypeConfiguration<Specialization>
{
    public void Configure(EntityTypeBuilder<Specialization> builder)
    {
        builder.ToTable("Specialization");

        builder.HasKey(p => p.Id);

        builder.Property(s => s.Id)
            .HasColumnName("SpecializationID")
            .IsRequired()
            .HasConversion(
                s => s.AsLong(),
                s => new SpecializationID(s))
            .ValueGeneratedOnAdd();

        builder.Property(s => s.specializationName)
            .IsRequired()
            .HasConversion(
                s => s.ToString(),
                s => new SpecializationName(s)
            )
            .HasMaxLength(100);
        
        builder.Property(s => s.specializationCode)
            .IsRequired()
            .HasConversion(
                s => s.ToString(),
                s => new SpecializationCode(s)
            )
            .HasMaxLength(10);
        
        builder.HasIndex(s => s.specializationCode)
            .IsUnique();
        
        builder.Property(s => s.specializationDescription)
            .IsRequired()
            .HasConversion(
                s => s.ToString(),
                s => new SpecializationDescription(s)
            )
            .HasMaxLength(500);

        builder.HasIndex(s => s.specializationName)
            .IsUnique();
        
        builder.Property(p => p.specializationStatus)
            .IsRequired()
            .HasConversion(
                p => p.ToString(),
                p => (SpecializationStatusEnum)Enum.Parse(typeof(SpecializationStatusEnum), p)
            );

    }
}