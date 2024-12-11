using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using Sempi5.Domain;
using Sempi5.Domain.AccoutToDeleteAggregate;
using Sempi5.Domain.PersonalData;
using Sempi5.Domain.User;

namespace Sempi5.Infrastructure.AccoutToDeleteRepository;

public class RoomAgendaEntityTypeConfiguration : IEntityTypeConfiguration<RoomAgenda>
{
    public void Configure(EntityTypeBuilder<RoomAgenda> builder)
    {
        builder.HasKey(ra => ra.Id);

        builder.Property(t => t.Id)
            .HasColumnName("RommAgendaID")
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