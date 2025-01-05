import {MedicalRecordFreeText} from "../../../../src/domain/MedicalRecordFreeText/MedicalRecordFreeText";

it('should create a MedicalRecordFreeText successfully', ()=>{
    const props={
        medicalRecord:'20250100005',
        doctorId:'N202400005',
        comment:'Bad breath'
    };

    const result=MedicalRecordFreeText.create(props);

    expect(result.isSuccess).toBe(true);
    expect(result.getValue()).toBeInstanceOf(MedicalRecordFreeText);

    const medicalRecordFreeText = result.getValue();
    expect(medicalRecordFreeText.medicalRecord).toBe('20250100005');
    expect(medicalRecordFreeText.doctorId).toBe('N202400005');
    expect(medicalRecordFreeText.comment).toBe('Bad breath');

});

it('should return correct values using getters', ()=>{
    const props={
        medicalRecord:'20250100005',
        doctorId:'N202400005',
        comment:'He is a drug addict'
    };
    const result=MedicalRecordFreeText.create(props);
    const medicalRecordFreeText = result.getValue();

    expect(medicalRecordFreeText.medicalRecord).toBe('20250100005');
    expect(medicalRecordFreeText.doctorId).toBe('N202400005');
    expect(medicalRecordFreeText.comment).toBe('He is a drug addict');


})