import {MedicalCondition} from './MedicalCondition';

describe('MedicalCondition Domain Tests', () => {
  let medicalCondition: MedicalCondition;

  beforeEach(() => {
    medicalCondition = {
      domainId: 'MC001',
      code: 'MC123',
      designation: 'Hypertension',
      description: 'High blood pressure',
      symptomsList: ['Headache', 'Blurred Vision', 'Fatigue'],
    };
  });

  it('should create a MedicalCondition object successfully', () => {
    expect(medicalCondition).toBeDefined();
    expect(medicalCondition.domainId).toBe('MC001');
    expect(medicalCondition.code).toBe('MC123');
    expect(medicalCondition.designation).toBe('Hypertension');
    expect(medicalCondition.description).toBe('High blood pressure');
    expect(medicalCondition.symptomsList).toEqual([
      'Headache',
      'Blurred Vision',
      'Fatigue',
    ]);
  });
});
