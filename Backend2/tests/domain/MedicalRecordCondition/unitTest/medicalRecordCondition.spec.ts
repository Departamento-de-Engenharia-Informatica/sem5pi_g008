import {MedicalRecordCondition} from "../../../../src/domain/MedicalRecordCondition/MedicalRecordCondition";

describe('MedicalRecordCondition', () => {

    it('should create a MedicalRecordCondition successfully', () => {
        const props = {
            condition: 'Hypertension',
            medicalRecord: 'MR001',
            doctorId: 'D123',
            comment: 'Needs regular checkup',
        };

        const result = MedicalRecordCondition.create(props);

        expect(result.isSuccess).toBe(true);
        expect(result.getValue()).toBeInstanceOf(MedicalRecordCondition);

        const medicalRecordCondition = result.getValue();
        expect(medicalRecordCondition.condition).toBe('Hypertension');
        expect(medicalRecordCondition.medicalRecord).toBe('MR001');
        expect(medicalRecordCondition.doctorId).toBe('D123');
        expect(medicalRecordCondition.comment).toBe('Needs regular checkup');
    });

    it('should return correct values using getters', () => {
        const props = {
            condition: 'Diabetes',
            medicalRecord: 'MR002',
            doctorId: 'D124',
            comment: 'Prescribed medication',
        };

        const result = MedicalRecordCondition.create(props);
        const medicalRecordCondition = result.getValue();

        expect(medicalRecordCondition.condition).toBe('Diabetes');
        expect(medicalRecordCondition.medicalRecord).toBe('MR002');
        expect(medicalRecordCondition.doctorId).toBe('D124');
        expect(medicalRecordCondition.comment).toBe('Prescribed medication');
    });

});
