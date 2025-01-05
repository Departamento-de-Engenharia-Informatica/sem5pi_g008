using System;
using System.Collections.Generic;
using Sempi5.Domain.OperationTypeAggregate;
using Sempi5.Domain.RequiredStaffAggregate;
using Sempi5.Domain.SpecializationAggregate;
using Xunit;
using Assert = Xunit.Assert;

namespace Sempi5.Tests.Domain.OperationTypeAggregate.Integration;

public class OperationTypeTest
{
    private OperationType CreateOperationType()
    {
        var opName = new OperationName("Teste");
        var setupDuration = TimeSpan.Parse("00:30:00");
        var surgeryDuration = TimeSpan.Parse("02:00:00");
        var cleaningDuration = TimeSpan.Parse("00:30:00");

        return new OperationType(opName, setupDuration, surgeryDuration, cleaningDuration);
    }

    [Fact]
    public void TestValidConstructor1()
    {
        // Arrange
        var opName = new OperationName("Teste");
        var numStaff = new NumberOfStaff(2);
        var specializationName = new SpecializationName("surgery");
        var specializationCode = new SpecializationCode("C1");
        var specializationDescription = new SpecializationDescription("Especialização em Cardiologia");
        var specialization = new Specialization(specializationName, specializationCode, specializationDescription);
        var reqStaff = new RequiredStaff(numStaff, specialization);
        var reqStaffList = new List<RequiredStaff> { reqStaff };
        var setupDuration = TimeSpan.Parse("00:30:00");
        var surgeryDuration = TimeSpan.Parse("02:00:00");
        var cleaningDuration = TimeSpan.Parse("00:30:00");
        
        // Act
        var obj = new OperationType(opName, reqStaffList, setupDuration, surgeryDuration, cleaningDuration);
        
        // Assert
        Assert.NotNull(obj);
    }
    
    [Fact]
    public void TestValidConstructor2()
    {
        // Arrange
        var opName = new OperationName("Teste");
        var setupDuration = TimeSpan.Parse("00:30:00");
        var surgeryDuration = TimeSpan.Parse("02:00:00");
        var cleaningDuration = TimeSpan.Parse("00:30:00");
        
        // Act
        var obj = new OperationType(opName, setupDuration, surgeryDuration, cleaningDuration);
        
        // Assert
        Assert.NotNull(obj);
    }

    [Fact]
    public void TestAddRequiredStaff()
    {
        // Arrange
        var opType = CreateOperationType();
        var numStaff = new NumberOfStaff(2);
        var specializationName = new SpecializationName("surgery");
        var specializationCode = new SpecializationCode("C1");
        var specializationDescription = new SpecializationDescription("Especialização em Cardiologia");
        var specialization = new Specialization(specializationName, specializationCode, specializationDescription);
        var reqStaff = new RequiredStaff(numStaff, specialization);

        // Act
        opType.AddRequiredStaff(reqStaff);
        
        // Assert
        Assert.Equal(1, opType.RequiredStaff.Count);
    }
    
    [Fact]
    public void TestRemoveRequiredStaff()
    {
        // Arrange
        var opType = CreateOperationType();
        var numStaff = new NumberOfStaff(2);
        var specializationName = new SpecializationName("surgery");
        var specializationCode = new SpecializationCode("C1");
        var specializationDescription = new SpecializationDescription("Especialização em Cardiologia");
        var specialization = new Specialization(specializationName, specializationCode, specializationDescription);
        var reqStaff = new RequiredStaff(numStaff, specialization);

        opType.AddRequiredStaff(reqStaff);
        
        // Act
        opType.RemoveRequiredStaff(reqStaff);
        
        // Assert
        Assert.Empty(opType.RequiredStaff);
    }
    
    [Fact]
    public void TestMarkAsNoLongerPerformed()
    {
        // Arrange
        var opType = CreateOperationType();
        
        // Act
        opType.MarkAsNoLongerPerformed();
        
        // Assert
        Assert.False(opType.stillPerformed);
    }
}
