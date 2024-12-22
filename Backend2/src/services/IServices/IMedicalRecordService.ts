import IMedicalRecordAllergyDTO from "../../dto/IMedicalRecordAllergyDTO";

export default interface IMedicalRecordService{
    createMedicalRecord(medicalRecordId: string): Promise<void>;
    getAllergies(medicalRecordId: string): Promise<IMedicalRecordAllergyDTO[]>;
    getMedicalRecordConditions(medicalRecordId: string): Promise<any>;
}
