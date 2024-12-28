import {Repo} from "../../core/infra/Repo";
import {MedicalRecordFreeText} from "../../domain/MedicalRecordFreeText/MedicalRecordFreeText";

export default interface IMedicalRecordFreeTextRepo extends Repo<MedicalRecordFreeText> {
  save(medicalRecordFreeText: MedicalRecordFreeText): Promise<MedicalRecordFreeText>;
}
