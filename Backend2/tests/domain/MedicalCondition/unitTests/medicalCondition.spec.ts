import {instance, mock, when} from 'ts-mockito';
import {MedicalCondition} from "../../../../src/domain/MedicalCondition/MedicalCondition";
import {Designation} from "../../../../src/domain/Shared/designation";
import {Code} from "../../../../src/domain/Shared/code";
import {Description} from "../../../../src/domain/Shared/description";


describe('MedicalCondition with mocks', () => {
  let mockCode: Code;
  let mockDesignation: Designation;
  let mockDescription: Description;
  let medicalConditionProps: any;

  beforeEach(() => {
    mockCode = mock(Code);
    mockDesignation = mock(Designation);
    mockDescription = mock(Description);

    when(mockCode.value).thenReturn('M123');
    when(mockDesignation.value).thenReturn('Diabetes');
    when(mockDescription.value).thenReturn('A chronic condition affecting blood sugar.');

    const codeInstance = instance(mockCode);
    const designationInstance = instance(mockDesignation);
    const descriptionInstance = instance(mockDescription);

    medicalConditionProps = {
      code: codeInstance,
      designation: designationInstance,
      description: descriptionInstance,
      symptomsList: ['Increased thirst', 'Frequent urination', 'Extreme hunger'],
    };
  });

  it('should create a MedicalCondition instance successfully', () => {
    const result = MedicalCondition.create(medicalConditionProps);
    expect(result.isSuccess).toBe(true);

    const medicalCondition = result.getValue();
    expect(medicalCondition).toBeInstanceOf(MedicalCondition);
    expect(medicalCondition.code).toEqual(medicalConditionProps.code);
    expect(medicalCondition.designation).toEqual(medicalConditionProps.designation);
    expect(medicalCondition.description).toEqual(medicalConditionProps.description);
    expect(medicalCondition.symptomsList).toEqual(medicalConditionProps.symptomsList);
  });

  it('should update the symptoms list of a medical condition', () => {
    const result = MedicalCondition.create(medicalConditionProps);
    expect(result.isSuccess).toBe(true);

    const medicalCondition = result.getValue();
    const newSymptoms = ['Blurred vision', 'Slow-healing sores', 'Frequent infections'];
    medicalCondition.updateSymptomsList(newSymptoms);

    expect(medicalCondition.symptomsList).toEqual(newSymptoms);
  });
  
  it('should update the description of a medical condition', () => {
    const result = MedicalCondition.create(medicalConditionProps);
    expect(result.isSuccess).toBe(true);
    
    const newMockDescription = mock(Description);
    when(newMockDescription.value).thenReturn('A chronic condition affecting blood sugar and insulin production.');

    const newDescription = instance(newMockDescription);
    
    const medicalCondition = result.getValue();

    medicalCondition.description = newDescription;
    
    expect(medicalCondition.description.value).toEqual('A chronic condition affecting blood sugar and insulin production.');
  });
  
});
