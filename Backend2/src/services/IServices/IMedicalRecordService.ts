import IMedicalRecordAllergyDTO from "../../dto/IMedicalRecordAllergyDTO";

export default interface IMedicalRecordService{
    createMedicalRecord(medicalRecordId: string): Promise<void>;
    getAllergies(medicalRecordId: string): Promise<IMedicalRecordAllergyDTO[]>;
    updateMedicalConditions(medicalRecordId: any, updatedConditions: any): any;
}
