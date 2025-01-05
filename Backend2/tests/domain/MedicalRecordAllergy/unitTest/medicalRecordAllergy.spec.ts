import {MedicalRecordAllergy} from "../../../../src/domain/MedicalRecordAllergy/MedicalRecordAllergy";

describe('MedicalRecordAllergy', () => {

    it('should create a MedicalRecordAllergy successfully', () => {
        const props = {
            allergyId: 'A001',
            medicalRecordId: 'MR001',
            doctorId: 'D123',
            comment: 'Needs regular checkup'
        };
        
        const result = MedicalRecordAllergy.create(props);
        
        expect(result.isSuccess).toBe(true);
        expect(result.getValue()).toBeInstanceOf(MedicalRecordAllergy);
        
        const medicalRecordAllergy = result.getValue();
        expect(medicalRecordAllergy.allergy).toBe('A001');
        expect(medicalRecordAllergy.medicalRecord).toBe('MR001');
        expect(medicalRecordAllergy.doctorId).toBe('D123');
        expect(medicalRecordAllergy.comment).toBe('Needs regular checkup');
    });
    
    it('should return correct values using getters', () => {
        const props = {
            allergyId: 'A002',
            medicalRecordId: 'MR002',
            doctorId: 'D124',
            comment: 'Prescribed medication'
        };
        
        const result = MedicalRecordAllergy.create(props);
        const medicalRecordAllergy = result.getValue();
        
        expect(medicalRecordAllergy.allergy).toBe('A002');
        expect(medicalRecordAllergy.medicalRecord).toBe('MR002');
        expect(medicalRecordAllergy.doctorId).toBe('D124');
        expect(medicalRecordAllergy.comment).toBe('Prescribed medication');
    });
});