import {MedicalRecordAllergy}  from './MedicalRecordAllergy';

describe('MedicalRecordAllergy Domain Tests', () => {
  let medicalRecordAllergy: MedicalRecordAllergy;

  beforeEach(() => {
    medicalRecordAllergy = {
      domainId: 'MRA001',
      allergy: 'AL10',
      medicalRecordId: 'MR001',
      doctor: 'Dr. John Doe',
      comment: 'Patient has a rash and itching.',
    };
  });

  it('should create a MedicalRecordAllergy object successfully', () => {
    expect(medicalRecordAllergy).toBeDefined();
    expect(medicalRecordAllergy.domainId).toBe('MRA001');
    expect(medicalRecordAllergy.allergy).toBe('AL10');
    expect(medicalRecordAllergy.medicalRecordId).toBe('MR001');
    expect(medicalRecordAllergy.doctor).toBe('Dr. John Doe');
    expect(medicalRecordAllergy.comment).toBe('Patient has a rash and itching.');
  });

});
