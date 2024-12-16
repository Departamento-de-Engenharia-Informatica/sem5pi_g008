import {Repo} from "../../core/infra/Repo";
import {MedicalRecord} from "../../domain/MedicalRecord/MedicalRecord";

export default interface IMedicalRecordRepo extends Repo<MedicalRecord>{
    save(medicalRecord: MedicalRecord): Promise<MedicalRecord>;

}