import {MedicalRecordFamilyHistory} from "../../../../src/domain/MedicalRecordFamilyHistory/MedicalRecordFamilyHistory";
import {UniqueEntityID} from "../../../../src/core/domain/UniqueEntityID";


describe('MedicalRecordFamilyHistory', () => {
    it('should create a valid MedicalRecordFamilyHistory instance', () => {
        // Arrange
        const props = {
            medicalRecord: 'record-123',
            familyMember: 'Father',
            condition: 'Diabetes',
        };

        // Act
        const result = MedicalRecordFamilyHistory.create(props);

        // Assert
        expect(result.isSuccess).toBe(true);
        const familyHistory = result.getValue();
        expect(familyHistory).toBeDefined();
        expect(familyHistory.medicalRecord).toBe('record-123');
        expect(familyHistory.familyMember).toBe('Father');
        expect(familyHistory.condition).toBe('Diabetes');
    });

    it('should fail to create MedicalRecordFamilyHistory with missing props', () => {
        // Arrange
        const props = {
            medicalRecord: '',
            familyMember: '',
            condition: '',
        };

        // Act
        const result = MedicalRecordFamilyHistory.create(props);

        // Assert
        expect(result.isSuccess).toBe(false);
    });

    it('should have a valid domainId', () => {
        // Arrange
        const props = {
            medicalRecord: 'record-456',
            familyMember: 'Mother',
            condition: 'Hypertension',
        };
        const id = new UniqueEntityID(1);

        // Act
        const result = MedicalRecordFamilyHistory.create(props, id);

        // Assert
        expect(result.isSuccess).toBe(true);
        const familyHistory = result.getValue();
        expect(familyHistory.domainId).toBe(1);
    });
});
