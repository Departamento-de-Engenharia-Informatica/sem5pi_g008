using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;
using Sempi5.Domain;
using Sempi5.Domain.SurgeryRoomAggregate;

namespace Sempi5.Infrastructure.SurgeryRoomRepository
{
    public class SurgeryRoomEntityTypeConfiguration : IEntityTypeConfiguration<SurgeryRoom>
    {
        public void Configure(EntityTypeBuilder<SurgeryRoom> builder)
        {
            builder.ToTable("SurgeryRoom");
            
            builder.HasKey(t => t.Id);
            
            builder.Property(t => t.Id)
                .HasColumnName("Id")
                .HasConversion(
                    v => v.AsInt(),  
                    v => new RoomNumber(v) 
                )
                .ValueGeneratedOnAdd();
                ;

            builder.Property(t => t.Type)
                .IsRequired()
                .HasConversion(
                    v => v.ToString(),
                    v => (RoomTypeEnum)Enum.Parse(typeof(RoomTypeEnum), v)
                );

            builder.Property(t => t.Status)
                .IsRequired()
                .HasConversion(
                    v => v.ToString(),
                    v => (RoomStatusEnum)Enum.Parse(typeof(RoomStatusEnum), v)
                );

            builder.Property(t => t.Capacity)
                .IsRequired()
                .HasConversion(
                    v => v.AsInt(),
                    v => new RoomCapacity(v)
                );
            builder.HasMany(t=>t.RoomAgendas)
                .WithOne()
                .HasForeignKey("SurgeryRoomID");
        }
    }
}
