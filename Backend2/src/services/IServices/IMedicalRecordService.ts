import IMedicalRecordAllergyDTO from "../../dto/IMedicalRecordAllergyDTO";

export default interface IMedicalRecordService{
    createMedicalRecord(medicalRecordId: string): Promise<void>;
    getAllergies(medicalRecordId: string): Promise<IMedicalRecordAllergyDTO[]>;
    getMedicalRecordConditions(medicalRecordId: string): Promise<any>;
    getMedicalRecordConditionByCode(medicalRecordId: string, conditionCode: string): Promise<any>;
    getMedicalRecordConditionByDesignation(medicalRecordId: string, conditionDesignation: string): Promise<any>;

    updateMedicalConditions(medicalRecordId: any, updatedConditions: any): any;
}
