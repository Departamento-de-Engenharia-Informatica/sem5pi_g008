using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using Sempi5.Domain;
using Sempi5.Domain.AccoutToDeleteAggregate;
using Sempi5.Domain.PersonalData;
using Sempi5.Domain.User;

namespace Sempi5.Infrastructure.AccoutToDeleteRepository;

public class StaffAgendaEntityTypeConfiguration : IEntityTypeConfiguration<StaffAgenda>
{
    public void Configure(EntityTypeBuilder<StaffAgenda> builder)
    {
        builder.HasKey(ra => ra.Id);

        builder.Property(t => t.Id)
            .HasColumnName("StaffAgendaID")
            .IsRequired()
            .HasConversion(
                v => v.AsLong(),
                v => new AgendaId(v))
            .ValueGeneratedOnAdd();
        builder.Property(t => t.Date)
            .HasColumnName("Date")
            .IsRequired();
        builder.Property(t => t.TimeIntervals)
            .HasColumnName("TimeIntervals")
            .IsRequired();
    }
    
}