export default interface IMedicalRecordConditionService {
    getMedicalRecordConditions(medicalRecordId: string): Promise<any>;
}