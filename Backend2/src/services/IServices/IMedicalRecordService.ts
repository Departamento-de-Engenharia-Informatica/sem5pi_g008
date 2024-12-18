
export default interface IMedicalRecordService{
    createMedicalRecord(medicalRecordId: string): Promise<void>;
}
