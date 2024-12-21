using System;
using Xunit;
using Sempi5.Domain.SpecializationAggregate;
using Assert = Xunit.Assert;

namespace Sempi5.Tests.Domain.SpecializationAggregate.Integration;

public class SpecializationTest
{
    [Fact]
    public void ConstructorTest()
    {
        // Arrange
        var specializationName = new SpecializationName("Test");
        var specializationCode = new SpecializationCode("C1");
        var specializationDescription = new SpecializationDescription("Especialização em Cardiologia");
        
        // Act
        var obj = new Specialization(specializationName, specializationCode, specializationDescription);
        
        // Assert
        Assert.NotNull(obj);
        Assert.Equal(specializationName, obj.specializationName);
    }
    
    [Fact]
    public void ConstructorWithNullSpecializationNameTest()
    {
        Assert.Throws<ArgumentNullException>(() => new Specialization(null,null,null));
    }
    
}