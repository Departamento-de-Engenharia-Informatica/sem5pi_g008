import {MedicalRecordCondition} from './MedicalRecordCondition';

describe('MedicalRecordCondition Domain Tests', () => {
  let medicalRecordCondition: MedicalRecordCondition;

  beforeEach(() => {
    medicalRecordCondition = {
      conditionId: 'COND001',
      conditionCode: 'C123',
      conditionDesignation: 'Hypertension',
      medicalRecordId: 'MR001',
      doctorName: 'Dr. John Doe',
      doctorLicenseNumber: 123456,
      comment: 'Patient shows symptoms of high blood pressure.',
    };
  });

  it('should create a MedicalRecordCondition object successfully', () => {
    expect(medicalRecordCondition).toBeDefined();
    expect(medicalRecordCondition.conditionId).toBe('COND001');
    expect(medicalRecordCondition.conditionCode).toBe('C123');
    expect(medicalRecordCondition.conditionDesignation).toBe('Hypertension');
    expect(medicalRecordCondition.medicalRecordId).toBe('MR001');
    expect(medicalRecordCondition.doctorName).toBe('Dr. John Doe');
    expect(medicalRecordCondition.doctorLicenseNumber).toBe(123456);
    expect(medicalRecordCondition.comment).toBe(
      'Patient shows symptoms of high blood pressure.'
    );
  });
});
